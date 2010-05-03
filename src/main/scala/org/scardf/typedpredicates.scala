package org.scardf

import NodeConverter._

/**
 * A UriRef that can be used as a predicate in triples,
 * with specific value type (corresponding to range of RDF properties).
 * Use method {#v} to create an appropriate {NodeToValueConverter}
 * (as UriRefs are {NodeToBagConverter}s).
 */
case class Property[T]( override val uri: String )( implicit nc: NodeToValueConverter[T] )
extends UriRef( uri ) {
  /**
   * Applies this predicate and node-to-value converter.
   * Predicate must yield exactly one node for the method to return value.
   */
  def valueOf( gn: GraphNode ): T = gn/this/nc

  /**
   * Creates new node-to-value converter applying {#valueOf} to graph node.
   */
  val v: NodeToValueConverter[T] = new GraphNodeConverter[T]{
    def convertGraphNode( gn: GraphNode ) = valueOf( gn )
  }
  
  val option = NodeBagConverter[Option[T]]( _/this match {
    case NodeBag( Nil, _ ) => None
    case NodeBag( List( n ), g ) => Some( n(g)/nc )
    case x => throw new RdfTraversalException( "Yielded multiple RDF nodes: " + x )
  } )
  
  val set: NodeBagConverter[Set[T]] = 
    NodeBagConverter[Set[T]]( n => Set.empty ++ (n/this).nodesFromGraph.map{ nc( _ ) } )
  
  val exist: NodeBagConverter[Boolean] = NodeBagConverter[Boolean]( nb => !(nb/this).isEmpty )
}

/**
 * Properties with range of subject nodes.
 * {#v} actually converts to {GraphNode}.
 * Use {#n} to convert to {SubjectNode}
 */
case class GNProperty( override val uri: String ) extends Property[GraphNode]( uri ) {
  /**
   * New node-to-SubjectNode converter using {#valueOf} method.
   * Use instead of "v(x).node"
   */
  val n: NodeToValueConverter[SubjectNode] = new GraphNodeConverter[SubjectNode]{
    def convertGraphNode( gn: GraphNode ) = valueOf( gn ).node
  }
}
