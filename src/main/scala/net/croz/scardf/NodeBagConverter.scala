package net.croz.scardf

import org.joda.time.LocalDate

/**
 * Apply method of a constructor will convert given node bag into some other object,
 * using the function given in constructor.
 */
class NodeBagConverter[T]( fn: NodeBag => T ) {
  def apply( bag: NodeBag ) = fn( bag )
}

/**
 * Converts bag to a single node, typically a literal value.
 */
class NodeConverter[T]( fn: Node => T )
extends NodeBagConverter[T]( bag => fn( bag.singleNode ) ) 
{
  /**
   * Constructs another converter which will return None when given an empty bag,
   * and apply this converter on some containing node otherwise.
   */
  def option = new NodeBagConverter[Option[T]]( bag => {
    val nopt = bag.nodeOption
    if ( nopt.isDefined ) Some( apply( nopt.get ) )
    else None
  } )

  /**
   * Constructs another converter which will return given default value for an empty bag,
   * and apply this converter on some containing node otherwise.
   */
  def default( defaultValue: T ) = new NodeBagConverter[T]( bag => {
    val nopt = bag.nodeOption
    if ( nopt.isDefined ) apply( nopt.get )
    else defaultValue
  } )

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
object asString extends NodeConverter[String]( _.asString )
object asBoolean extends NodeConverter[Boolean]( _.asBoolean )
object asInt extends NodeConverter[Int]( _.asInt )
object asLocalDate extends NodeConverter[LocalDate]( _.asLocalDate )
