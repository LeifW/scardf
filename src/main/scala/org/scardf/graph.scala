package org.scardf

import scala.collection.mutable.{Set => MSet}

trait Mutable

object Graph {
  def apply() = new SetGraph( Set.empty[Triple] )
  def apply( triples: Set[Triple] ): SetGraph = new SetGraph( triples )
  def build( branches: Branch* ): SetGraph = branches.map{ _.toGraph }.foldLeft( Graph() ){ _++_ }
}

trait Graph {
  def triples: Collection[Triple]
  def size = triples.size
  
  def +( t: Triple ): Graph
  def ++( ts: Iterable[Triple] ): Graph
  
  def ++( g: Graph ): Graph = this ++ g.triples
  
  /**
   * Two graphs are isomorphic if there is a mapping
   * between blank nodes in the graphs which makes two graph equal.
   * NOT FULLY IMPLEMENTED!
   */
  def =~( that: Graph ): Boolean = {
    if ( this.size != that.size ) return false
    val (thisBti, thisNbti) = triples partition{ _.hasBlankNode }
    val (thatBti, thatNbti) = that.triples partition{ _.hasBlankNode }
    val List( thisBt, thisNbt, thatBt, thatNbt ) = 
      List( thisBti, thisNbti, thatBti, thatNbti ) map { i => Set( i.toSeq: _* ) }
    if ( thisNbt != thatNbt ) return false
    val blankCount = this.blankNodes.toSeq.size
    if ( blankCount != that.blankNodes.toSeq.size ) return false
    if ( blankCount == 0 ) return true
    // probably isomorphic, but not sure: return true for now
    true
  }
  
  def contains( t: Triple ): Boolean
  
  def /( n: SubjectNode ) = GraphNode( n, this )
  def bagOf( vals: Any* ) = new NodeBag( vals map { Node from _ } toList, this )
  def /- = NodeBag( subjects.toList, this )
  def /-/( nc: NodeToBagConverter ): NodeBag = /-/( NodeConverter.toNodeBagConverter(nc) )
  def /-/[T]( bc: NodeBagConverter[T] ): T = (/-)/bc
  
  def subjects: Set[SubjectNode] = Set() ++ triples map { _.sub }
  def objects: Set[Node] = Set() ++ triples map { _.obj }
  def nodes = subjects ++ objects
  def blankNodes: Iterable[Blank] = nodes filter { _.isBlank } map { _.asInstanceOf[Blank] }
  
  def filterT( pf: PartialFunction[Triple, Boolean] ) = 
    Graph( Set() ++ triples filter { pf orElse {case _ => false} } )
  
  /**
   * Optional query engine available for querying this graph.
   */
  def queryEngineOpt: Option[QueryEngine] = None
  
  def renderIn( sf: SerializationFormat ): Serializator = sf match {
    case NTriple => new Serializator() {
      override def writeTo( w: java.io.Writer ) = null
      override def asString = triples.map{ _.rend }.mkString( "", "\n", "" )
    }
    case _ => throw new UnsupportedOperationException()
  }
  def rend: String = renderIn( NTriple ).asString
  
  override def toString = "Graph[ " + size + " triples ]"
}

class SetGraph( tset: Set[Triple] ) extends Graph {
  def triples = tset
  def +( t: Triple ): SetGraph = new SetGraph( Set( t ) ++ tset )
  def ++( ts: Iterable[Triple] ) = new SetGraph( tset ++ ts )
  override def ++( g: Graph ): SetGraph = this ++ g.triples
  override def contains( t: Triple ) = tset contains t
}

class MutableSetGraph() extends Graph with Mutable {
  val mset = MSet[Triple]()
  def triples = mset
  def +( t: Triple ): MutableSetGraph = { mset += t; this }
  def ++( ts: Iterable[Triple] ): MutableSetGraph = { mset ++= ts; this }
  override def ++( g: Graph ): MutableSetGraph = this ++ g.triples
  override def contains( t: Triple ) = mset contains t
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
