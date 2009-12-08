package net.croz.scardf

trait PredicateConstruct
  
object PropPath {
  def apply( props: Prop* ) = new PropPath( props.toList )
  implicit def toPropPath( prop: Prop ) = PropPath( prop )
}

/**
 * List of properties forming a path over resources in a graph,
 * a.k.a. a predicate chain.
 */
case class PropPath( propList: List[Prop] ) extends Seq[Prop] with PredicateChain {
  def ~( p: Prop ) = new PropPath( propList ::: List( p ) )
  def ~( pp: PropPath ) = new PropPath( propList ::: pp.propList )
  def ~( t: PredicateTree ) = PredicateTree.prependTree( this, t )
  def ~( t: PredicateTree, moretrees: PredicateTree* ): PredicateTree = 
    this ~ moretrees.foldLeft( t )( _ ++ _ )

  def prepend( other: PropPath ) = PropPath( other.propList ::: propList )
  def subpath( start: Int, finish: Int ) = PropPath( propList.slice( start, finish ).toList )
  override def drop(n: Int): PropPath = PropPath( propList drop n )
  
  def of( res: Res ) = apply( res )
  def apply( node: Node ) = node.asRes/this/!
  def update( res: Res, value: Any ) = if (!isEmpty)
    res/subpath( 0, propList.size-1 )/asRes state propList.last -> value
  
  val length = propList.length
  val elements = propList.elements
  def apply( index: Int ) = propList( index )
  
  override def toString = propList.mkString( "PChain( ", ", ", ")" )
}

trait PredicateChain extends PredicateConstruct {
  def apply( node: Node ): Node
  def update( res: Res, value: Any )
}
