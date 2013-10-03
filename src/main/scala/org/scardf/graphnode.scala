package org.scardf

import scala.collection.mutable.{Set => MSet}

case class GraphNode( node: SubjectNode, graph: Graph ) extends NodeFromGraph {
  override def /( ur: UriRef ) = 
    NodeBag( graph.triplesLike( node, ur, Node ).map{ _.obj }.toList, graph )
  
  def /[T]( nbc: NodeBagConverter[T] ): T = graph.bagOf( node )/nbc
  
  def -( pred: UriRef ) = SubPredPair( node, pred )
  
  def -( poPairs: Pair[ UriRef, Any ]* ) = node -( poPairs: _* )

  /**
   * Does node has given assignment in its graph?
   * has( null -> _ ) throws an IllegalArgumentException.
   * has( p -> null ) always yields true, for a not-null p.
   * Options are treated specially: has( p -> None ) returns true iff there are
   * NO valuesOf p in graph; has( p -> Some(o) ) is reduced to has( p -> o ).
   * @see #valuesOf
   */
  def has( a: Pair[ UriRef, Any ] ): Boolean = a match {
    case (null, _) => throw new IllegalArgumentException( "GraphNode.has requires predicate" )
    case (_, null) => true
    case (p, None) => valuesOf( p ).isEmpty
    case (p, Some( o )) => has( p -> o )
    case (p, o) => graph contains RdfTriple( node, p, Node from o )
  }

  /**
   * Iterable of all object nodes in this graph from triples
   * containing this node as subject and given UriRef as predicate.
   * @see #node
   */
  def valuesOf( predicate: UriRef ): Iterable[Node] =
    graph.triplesLike( node, predicate, Node ) map { _.obj }
  
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
  
  private def spreadTo( subgraph: MutableGraph, covered: MSet[SubjectNode] ) {
    println( subgraph.rend, covered )
    if ( covered contains this.node ) return
    else covered += this.node
    val outlinks = graph.triplesLike( this.node, Node, SubjectNode )
    val connectedNodes = MSet[GraphNode]()
    for ( s <- outlinks ) {
      connectedNodes += s.obj( graph ).asInstanceOf[GraphNode]
      subgraph + s
    }
    connectedNodes map { _.spreadTo( subgraph, covered ) }
  }
}
