package net.croz.scardf

object NodeBag {
  def apply( nodes: Node* ) = new NodeBag( nodes.toList )
  def apply( nodesIt: Iterator[Node] ) = new NodeBag( nodesIt.toList )
  implicit def toNodeBag( n: Node ) = new NodeBag( List( n ) )
  implicit def toNodeBag( nOpt: Option[Node] ) = new NodeBag( nOpt.toList )
}

/**
 * An unordered collection of nodes.
 */
class NodeBag( val list: List[Node] ) extends scala.Iterable[Node] {
  
  /**
   * Retrieves a node from this bag. It could be any one node contained in the bag.
   * @throws NoSuchElementException if the bag is empty
   * @see #nodeOption
   */
  def oneNode = list.head
  
  /**
   * Retrieves some node inside this bag, or None if the bag is empty.
   * @see #oneNode
   */
  def nodeOption: Option[Node] = if ( list.isEmpty ) None else Some( oneNode )
  
  /**
   * Retrieves the single node contained in this bag.
   * @throws RdfTraversalException if the size of this bag is not 1
   */
  def singleNode =
    if ( list.isEmpty ) throw new RdfTraversalException( "Yielded no RDF node" )
    else if ( list.size > 1 ) throw new RdfTraversalException( "Yielded multiple RDF nodes: " + list )
    else oneNode 

  /**
   * Bag of all property values for given predicate of all nodes in this bag.
   */
  def /( predicate: Prop ): NodeBag = 
    new NodeBag( list flatMap { _.asRes.valuesOf( predicate ).toList } )
  
  /**
   * Applies given converter to this bag yielding a set of values.
   */
  def /[T]( converter: NodeBagConverter[T] ): T = converter( this )

  /**
  * Filters this bag for nodes that are literals in the given language.
  */
  def /( lang: Lang ): NodeBag = this/where( _ isLitOn lang )

  /**
   * All nodes in the bag are convertable to boolean "true".
   * For an empty bag returns false.
   */
  def ? = this/asBoolean.set == Set( true )
  
  /**
   * Does this bag contain any nodes? Inverse of {@link #isEmpty}
   */
  def /? = !list.isEmpty
  
  /**
   * Alias for {@link #singleNode}
   */
  def /! = singleNode
  
  /**
   * Simple string representation of the lexical values of all nodes in bag, separated with spaces.
   */
  def % = list.map( _.lexic ).mkString( "", " ", "" )

  override def iterator = list.iterator
  override def size = list.length
  def contains( n: Node ) = list contains n
  
  lazy val sorted = new NodeBag( Node sort list )

  /**
   * Equal if argument is a NodeBag with the same elements, regardless of order.
   */
  override def equals( o: Any ) = o match {
    case that: NodeBag => this.sorted.list sameElements that.sorted.list
    case _ => false
  }
  
  override lazy val hashCode: Int = this.sorted.hashCode
  
  override def toString = list.mkString( "NodeBag(", ", ", ")" )
}

class RdfTraversalException( msg: String ) extends Exception( msg )
