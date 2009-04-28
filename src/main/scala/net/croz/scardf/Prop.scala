package net.croz.scardf

import com.hp.hpl.jena.rdf.model.{Model => JModel}
import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.datatypes.RDFDatatype
import com.hp.hpl.jena.datatypes.TypeMapper

class Prop( val jProperty: Property, m: Model ) extends Res( jProperty, m ) with PredicateChain
{
  def this( jProperty: Property ) = this( jProperty, Model( jProperty.getModel ) )
  def of( res: Res ) = res/this/!
  def ? = new NodeConverter[Boolean]( x => (x/this/!).asBoolean )
  //def ->>( values: Any* ) = for ( v <- values ) yield (this, v)
  def -( p: Prop ) = PropPath( this, p )
  def -( subtrees: PredicateTree* ) = PredicateTree( this -> subtrees.reduceLeft( _ ++ _ ) )

  def apply( node: Node ): Node = node.asRes/this/!
  def update( res: Res, value: Any ) = res state this -> value
    
  override def assign( prop: Prop, value: Any ): Prop = {
    super.assign( prop, value )
    this
  }
  
  override def in( m: Model ) = m getProp jProperty
  def withRange( r: Res ) = { assign( RDFS.range, r ); this }

  def datatype: Option[RDFDatatype] = {
    val ranges = this/RDFS.range
    if ( !ranges.isEmpty ) {
      val range = ranges.oneNode.asRes
      if ( range isOfType RDFS.Datatype ) {
        val dtype = TypeMapper.getInstance.getTypeByName( range.uri )
        if ( dtype != null ) return Some( dtype )
      }
    }
    None
  }
}

object Prop {
  def apply( uri: String )( implicit rmodel: Model ) = rmodel getProp uri
  def apply( p: Property ): Prop = apply( p, Model( p.getModel ) )
  def apply( p: Property, m: Model ): Prop = m getProp p

  implicit def toProp( p: Property ): Prop = Prop( p )
  implicit def toPropAnyPair( pa: Pair[Property, Any] ): Pair[Prop, Any] = (toProp( pa._1 ), pa._2)
  implicit def toJProperty( p: Prop ): Property = p.jProperty
}
