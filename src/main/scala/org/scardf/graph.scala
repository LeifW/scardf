package org.scardf

import collection.mutable.{Set => MSet, Map => MMap}
import scala.language.postfixOps

/**
 * Factory for a SetGraph.
 *
 */
object Graph {
  def apply() = new SetGraph( Set.empty[RdfTriple] )
  def apply( triples: Set[RdfTriple] ): SetGraph = new SetGraph( triples )
  def apply( branches: Branch* ): SetGraph = build( branches: _* )
  
  /**
   * Builds a SetGraph from a sequence of branches.
   * @param branches
   * @return constructed graph
   */
  def build( branches: Branch* ): SetGraph = branches.map{ _.toGraph }.foldLeft( Graph() ){ _++_ }
}

/**
 * An RDF graph. Essentially a set of triples.
 * 
 */
trait Graph {
  def triples: Iterable[RdfTriple]
  def size = triples.size
  
  /**
   * 
   * @param triple triple to add
   * @return new graph containing all triples in this and also the given triple
   */
  def +( triple: RdfTriple ): Graph
  
  /**
   * Creates a new graph by adding all RDF triples to the triples in this graph.
   * @param triples a traversable collection of RDF triples to add
   * @return new graph containing a union of this and given triples
   */
  def ++( triples: TraversableOnce[RdfTriple] ): Graph

  /**
   * this ++ g.triples
   */
  def ++( g: Graph ): Graph = this ++ g.triples

  /**
   * Two graphs are isomorphic if there is a mapping
   * between blank nodes in the graphs which makes the two graphs equal.
   * NOT FULLY IMPLEMENTED!
   */
  def =~( that: Graph ): Boolean = {
    if ( this.size != that.size ) return false
    val blankCount = this.blankNodes.toSeq.size
    if ( blankCount != that.blankNodes.toSeq.size ) return false
    if ( blankCount == 0 ) return true
    val (thisBti, thisNbti) = triples partition{ _.hasBlankNode }
    val (thatBti, thatNbti) = that.triples partition{ _.hasBlankNode }
    val List( thisBt, thisNbt, thatBt, thatNbt ) = 
      List( thisBti, thisNbti, thatBti, thatNbti ) map { i => Set( i.toSeq: _* ) }
    if ( thisNbt != thatNbt ) return false
    true //TBD not implemented yet
    //Isomorphism.mapping( this, that ).isDefined
  }
  
  def contains( t: RdfTriple ): Boolean
  
  def /( n: SubjectNode ) = GraphNode( n, this )
  def bagOf( vals: Any* ) = new NodeBag( vals map { Node from _ } toList, this )
  def /- = NodeBag( subjects.toList, this )
  def /-/( nc: NodeToBagConverter ): NodeBag = nc match {
    // optimized for explicit predicates
    case pred: UriRef => new NodeBag( triplesLike( Node, pred, Node ).toList.map( _.obj ), this )
    case _ => /-/( NodeConverter.toNodeBagConverter(nc) )
  }
  def /-/[T]( bc: NodeBagConverter[T] ): T = (/-)/bc
  
  def subjects: Set[SubjectNode] = Set() ++ triples map { _.subj }
  def objects: Set[Node] = Set() ++ triples map { _.obj }
  def nodes = subjects ++ objects
  def blankNodes: Iterable[Blank] = (
    triplesLike( Blank, Node, Node ).map( _.subj.asInstanceOf[Blank] ) ++ 
    triplesLike( Node, Node, Blank ).map( _.obj.asInstanceOf[Blank] )
  )
  
  def triplesMatching( pf: PartialFunction[RdfTriple, Boolean] ): Iterable[RdfTriple] = 
    triples filter{ pf orElse {case _ => false} }

  /**
   * Matches triples by places, using pattern objects for {@link Node#matches}.
   * Note: This method is the preferred way of filtering triples in graph because it's declarative.
   * Implementations can use this information to optimize the filtering process, 
   * e.g. by using indices or modifying query strings.
   * @param sp pattern for the subject part
   * @param pp pattern for the predicate
   * @param op pattern for the object
   * @return an iterable with all triples in graph matching the parameters.
   * @see {Node#matches}
   */
  def triplesLike( sp: Any, pp: Any, op: Any ): Iterable[RdfTriple] = {
    import Node.matches
    triplesMatching { 
      case RdfTriple( s, p, o ) => matches( sp, s ) && matches( pp, p ) && matches( op, o ) 
    }
  }
  
  def rend: String = new Serializator( NTriple ).asString( this )
}

/**
 * Graphs with mutable state.
 */
trait MutableGraph extends Graph {
  /**
   * Adds a triple to this collection, mutating it.
   * @param t the triple
   * @return this
   */
  def +=( t: RdfTriple ): Graph

  /**
   * Adds triples to this collection, mutating it.
   * @param ts a traversable collection of RDF triples to add
   * @return this
   */
  def ++=( ts: TraversableOnce[RdfTriple] ): Graph

  /**
   * this ++= g.triples
   */
  def ++=( g: Graph ): Graph = this ++= g.triples
}

/**
 * Adds triples index to the in-memory graph.
 * Implements triplesLike to work
 */
trait IndexedGraph extends Graph {
  val index = new NodeIndex()

  override def triplesLike( sp: Any, pp: Any, op: Any ): Iterable[RdfTriple] = {
    import Node.matches
    val tt = 
      if ( !sp.isInstanceOf[Node] && !pp.isInstanceOf[Node] && !op.isInstanceOf[Node] ) triples
      else Set.empty ++ index( 1, sp ) ++ index( 2, pp ) ++ index( 3, op )
    tt filter {
      case RdfTriple( s, p, o ) => matches( sp, s ) && matches( pp, p ) && matches( op, o ) 
      case _ => false
    }
  }
}

/**
 * Immutable RDF graph stored in memory.
 */
class SetGraph( tset: Set[RdfTriple] ) extends IndexedGraph {
  tset foreach { index store }
  def triples = tset
  def +( t: RdfTriple ): SetGraph = new SetGraph( Set( t ) ++ tset )
  def ++( ts: TraversableOnce[RdfTriple] ) = new SetGraph( tset ++ ts )
  override def ++( g: Graph ): SetGraph = this ++ g.triples
  override def contains( t: RdfTriple ) = tset contains t
  
  override def toString = "SetGraph[ " + size + " triples ]"
}

/**
 * Triples can be added to this graph by mutating its triples set.
 * Not thread-safe.
 */
class MutableSetGraph() extends IndexedGraph with MutableGraph {
  val mset = MSet[RdfTriple]()
  def triples = mset //TODO make read-only view
  
  def +( t: RdfTriple ): MutableSetGraph = this ++ List( t )

  def +=( t: RdfTriple ): MutableSetGraph = {
    mset += t
    index store t
    this
  }

  override def ++=( ts: TraversableOnce[RdfTriple] ): MutableSetGraph = {
    mset ++= ts
    ts foreach { index store }
    this 
  }

  override def ++( ts: TraversableOnce[RdfTriple] ): MutableSetGraph =
    new MutableSetGraph() ++= this.mset ++= ts

  override def ++( g: Graph ): MutableSetGraph = this ++ g.triples
  override def contains( t: RdfTriple ) = mset contains t
  
  override def toString = "MutableSetGraph[ " + size + " triples ]"
}

class NodeIndex {
  type IndexMap = MMap[Node, MSet[RdfTriple]]
  def newIndexMap = MMap[Node, MSet[RdfTriple]]()
  val map = Map( 1 -> newIndexMap, 2 -> newIndexMap, 3 -> newIndexMap ) 
  
  def this( it: Iterable[RdfTriple] ) = {
    this()
    it foreach { store }
  }
  
  def apply( pos: Int, p: Any ): Iterable[RdfTriple] = p match {
    case n: Node => triples( pos, n )
    case _ => Nil
  }
  
  def store( t: RdfTriple ) = {
    update( 1, t.subj, t )
    update( 2, t.pred, t )
    update( 3, t.obj, t )
  }
  
  private[this] def update( pos: Int, n: Node, t: RdfTriple ) = 
    map( pos ).getOrElseUpdate( n, MSet[RdfTriple]() ) += t
  
  def triples( pos: Int, n: Node ) = map( pos ).getOrElse( n, MSet[RdfTriple]() )
}

/**
 * Writes graph to a Writer and reads from a Reader based on a given format.
 * This class only writes in NTriple format. 
 * All other options throw an UnsupportedOperationException.
 * Prefixes may be registered with the {#prefixes} method.
 */
class Serializator( sf: SerializationFormat ) {
  var bindings = Map[String, String]()

  def prefixes( pairs: (Symbol, Vocabulary)* ): Serializator = { 
    bindings = Map(
      ( pairs map { p => p._1.name -> p._2.prefix } ): _* 
    )
    this
  }

  def throwUnsupported = throw new UnsupportedOperationException( "Serialization format not supported" )

  def write( g: Graph, w: java.io.Writer ): Unit = sf match {
    case NTriple => g.triples foreach { t => w write ( t.rend ) }
    case _ => throwUnsupported
  }

  def asString( g: Graph ): String = {
    val sw = new java.io.StringWriter
    write( g, sw )
    sw.toString
  }
  
  def readFrom( r: java.io.Reader ): Graph = sf match {
    case NTriple => NTriplesParser(r)
    case _ => throwUnsupported
  }
}

abstract class SerializationFormat
object Turtle extends SerializationFormat
object NTriple extends SerializationFormat
object RdfXml extends SerializationFormat
object N3 extends SerializationFormat
object JsonLD extends SerializationFormat
