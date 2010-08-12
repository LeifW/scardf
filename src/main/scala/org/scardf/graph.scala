package org.scardf

import collection.mutable.{Set => MSet, Map => MMap}

trait Mutable

object Graph {
  def apply() = new SetGraph( Set.empty[Triple] )
  def apply( triples: Set[Triple] ): SetGraph = new SetGraph( triples )
  def build( branches: Branch* ): SetGraph = branches.map{ _.toGraph }.foldLeft( Graph() ){ _++_ }

  /**
   * NOT IMPLEMENTED!
   */
  def mapping( g1: Set[Triple], g2: Set[Triple] ): Option[Map[Blank, Blank]] = {
    val s1 = MSet() ++ g1
    val s2 = MSet() ++ g2
    
    Some(Map())
  }
}

trait Graph {
  def triples: Collection[Triple]
  def size = triples.size
  
  def +( t: Triple ): Graph
  def ++( ts: Iterable[Triple] ): Graph
  
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
    Graph.mapping( thisBt, thatBt ).isDefined
  }
  
  def contains( t: Triple ): Boolean
  
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
  def blankNodes: Iterable[Blank] = nodes filter { _.isBlank } map { _.asInstanceOf[Blank] }
  
  def triplesMatching( pf: PartialFunction[Triple, Boolean] ): Iterable[Triple] = 
    triples filter{ pf orElse {case _ => false} }

  def triplesLike( sp: Any, pp: Any, op: Any ): Iterable[Triple] = {
    import Node.matches
    triplesMatching { case Triple( s, p, o ) => matches( sp, s ) && matches( pp, p ) && matches( op, o ) }
  }
  
  /**
   * Optional query engine available for querying this graph.
   */
  def queryEngineOpt: Option[QueryEngine] = None
  
  def renderIn( sf: SerializationFormat ): Serializator = sf match {
    case NTriple => new Serializator() {
      override def writeTo( w: java.io.Writer ) = w write asString
      override def asString = triples.map{ _.rend }.mkString( "\n" )
    }
    case _ => throw new UnsupportedOperationException()
  }
  def rend: String = renderIn( NTriple ).asString
  
  override def toString = "Graph[ " + size + " triples ]"
}

trait IndexedGraph extends Graph {
  val index = new NodeIndex()

  override def triplesLike( sp: Any, pp: Any, op: Any ): Iterable[Triple] = {
    import Node.matches
    val tt = 
      if ( !sp.isInstanceOf[Node] && !pp.isInstanceOf[Node] && !op.isInstanceOf[Node] ) triples
      else Set.empty ++ index( 1, sp ) ++ index( 2, pp ) ++ index( 3, op )
    tt filter {
      case Triple( s, p, o ) => matches( sp, s ) && matches( pp, p ) && matches( op, o ) 
      case _ => false
    }
  }
}

class SetGraph( tset: Set[Triple] ) extends IndexedGraph {
  tset foreach { index store }
  def triples = tset
  def +( t: Triple ): SetGraph = new SetGraph( Set( t ) ++ tset )
  def ++( ts: Iterable[Triple] ) = new SetGraph( tset ++ ts )
  override def ++( g: Graph ): SetGraph = this ++ g.triples
  override def contains( t: Triple ) = tset contains t
}

/**
 * Triples can be added to this graph by mutating its triples set.
 * Not thread-safe.
 */
class MutableSetGraph() extends IndexedGraph with Mutable {
  val mset = MSet[Triple]()
  def triples = mset
  
  def +( t: Triple ): MutableSetGraph = {
    mset += t
    index store t
    this 
  }
  
  def ++( ts: Iterable[Triple] ): MutableSetGraph = {
    mset ++= ts
    ts foreach { index store }
    this 
  }
  
  override def ++( g: Graph ): MutableSetGraph = this ++ g.triples
  override def contains( t: Triple ) = mset contains t
}

class NodeIndex {
  type IndexMap = MMap[Node, MSet[Triple]]
  def newIndexMap = MMap[Node, MSet[Triple]]()
  val map = Map(1 -> newIndexMap, 2 -> newIndexMap, 3 -> newIndexMap ) 
  
  def this( it: Iterable[Triple] ) = {
    this()
    it foreach { store }
  }
    
  def apply( pos: Int, p: Any ): Iterable[Triple] = p match {
    case n: Node => triples( pos, n )
    case _ => Nil
  }
  
  def store( t: Triple ) = {
    update( 1, t.subj, t )
    update( 2, t.pred, t )
    update( 3, t.obj, t )
  }
  
  private[this] def update( pos: Int, n: Node, t: Triple ) = 
    map( pos ).getOrElseUpdate( n, MSet[Triple]() ) + t
  
  def triples( pos: Int, n: Node ) = map( pos ).getOrElse( n, MSet[Triple]() )
}

abstract class Serializator {
  var bindings: Map[String, String] = Map()
  
  def prefixes( pairs: Pair[Symbol, Vocabulary]* ): Serializator = { 
    bindings = Map(
      ( pairs map { p => p._1.name -> p._2.prefix } ): _* 
    )
    this
  }
  
  def writeTo( w: java.io.Writer ): Unit = throw new UnsupportedOperationException()
  
  def asString: String = {
    val sw = new java.io.StringWriter
    writeTo( sw )
    sw.toString
  }
  
  def readFrom( r: java.io.Reader ): Graph = throw new UnsupportedOperationException()
}

abstract class SerializationFormat
object Turtle extends SerializationFormat
object NTriple extends SerializationFormat
object RdfXml extends SerializationFormat
object N3 extends SerializationFormat
