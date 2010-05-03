package org.scardf

import scala.collection.mutable.{Set => MSet}

case class GraphNode( node: SubjectNode, graph: Graph ) extends NodeFromGraph {
  override def /( ur: UriRef ) = 
    NodeBag( graph.triples.filter{ t => t.sub == node && t.pred == ur }.map{ _.obj }.toList, graph )
  
  def /[T]( nbc: NodeBagConverter[T] ): T = graph.bagOf( node )/nbc
  
  def -( pred: UriRef ) = SubPredPair( node, pred )
  
  def -( poPairs: Pair[ UriRef, Any ]* ) = node -( poPairs: _* )

  def has( a: Pair[ UriRef, Any ] ) = a match {
    case (p, None) => valuesOf( p ).isEmpty
    case _ => graph contains Triple( node, a._1, Node from a._2 )
  }

  def valuesOf( predicate: UriRef ): Iterable[Node] =
    graph.triples filter { 
      case Triple( `node`, `predicate`, o ) => true
      case _ => false
    } map { _.obj }
  
  /**
   * Subgraphed node N in G is the same N in another graph S which is a subgraph of G
   * and contains all of the triples that are reachable from N in G.
   * These are all triples with N in subject, 
   * as well as subgraphs from all resources that were objects of those triples.
   */
  def subgraphed = {
    val g = new MutableSetGraph()
    spreadTo( g, MSet[SubjectNode]() )
    g/node
  }
  
  private def spreadTo( subgraph: Graph with Mutable, covered: MSet[SubjectNode] ) {
    println( subgraph.rend, covered )
    if ( covered contains this.node ) return
    else covered += this.node
    val outlinks = graph.filterT{ case Triple( this.node, _, _ ) => true }.triples
    val connectedNodes = MSet[GraphNode]()
    for ( s <- outlinks ) {
      if ( !s.obj.isLiteral ) connectedNodes += s.obj( graph ).asInstanceOf[GraphNode]
      subgraph + s
    }
    connectedNodes map { _.spreadTo( subgraph, covered ) }
  }
}
