package net.croz.scardf

import org.joda.time.{ LocalDate, DateTime }

/**
 * Apply method converts given node bag into some other object,
 * using the function given in constructor.
 */
class NodeBagConverter[T]( fn: NodeBag => T ) extends (NodeBag => T) {
  def apply( bag: NodeBag ) = fn( bag )
}

/**
 * Converts a single-node bag to an object of type T (typically a literal value),
 * given a Node => T function.
 * Apply method throws an RdfTraversalException if the size of the bag is not 1.
 */
class NodeConverter[T]( fn: Node => T )
extends NodeBagConverter[T]( bag => fn( bag.singleNode ) ) 
{
  /**
   * Constructs another converter which will return None when given an empty bag,
   * and apply this converter on the single node otherwise.
   */
  def option = new NodeBagConverter[Option[T]]( bag => {
    if ( bag.isEmpty ) None
    else if ( bag.size > 1 ) throw new RdfTraversalException( "Yielded multiple RDF nodes: " + bag )
    else Some( this( bag.singleNode ) )
  } )

  /**
   * Constructs another converter which will return given default value for an empty bag,
   * and apply this converter on the single node otherwise.
   */
  def default( defaultValue: T ) = new NodeBagConverter[T]( bag => option( bag ) getOrElse defaultValue )

  /**
   * Constructs another bag converter which will return an Iterable through all values 
   * converted from the nodes in given bag using this node converter.
   */
  def iterable = new NodeBagConverter[ Iterable[T] ]( _.map { this( _ ) } )

  /**
   * Constructs another bag converter which will return a set of all values converted from 
   * the nodes in given bag using this node converter. 
   */
  def set = new NodeBagConverter[ Set[T] ]( bag => Set.empty ++ bag.map { this( _ ) } )
}

object asRes extends NodeConverter[Res]( _.asRes )
object asProp extends NodeConverter[Prop]( _.asProp )
object asLit extends NodeConverter[Lit]( _.asLit )
object asString extends NodeConverter[String]( _.asString )
object asBoolean extends NodeConverter[Boolean]( _.asBoolean )
object asInt extends NodeConverter[Int]( _.asInt )
object asDouble extends NodeConverter[Double]( _.asDouble )
object asBigDecimal extends NodeConverter[BigDecimal]( _.asBigDecimal )
object asLocalDate extends NodeConverter[LocalDate]( _.asLocalDate )
object asDateTime extends NodeConverter[DateTime]( _.asDateTime )

/** Converts single node to a list. */
object asRdfList extends NodeConverter[RdfList] ( _.asRdfList )

/** Converts single node to a new bag containing its list elements. */
object asRdfListBag extends NodeConverter[NodeBag] ( _.asRdfList.toNodeBag )

/**
 * Converts a node bag to another node bag by filtering its nodes.
 */
class NodeBagFilter( ffn: Node => Boolean )
extends NodeBagConverter[NodeBag]( bag => new NodeBag( bag.list filter ffn ) )

/**
 * Factory object for bag filters.
 * @see NodeBagFilter
 */
object where {
  def apply( ffn: Node => Boolean ) = new NodeBagFilter( ffn )
  def apply( assignment: Pair[ Prop, Any ] ) = new NodeBagFilter( _.asRes has assignment )
}
