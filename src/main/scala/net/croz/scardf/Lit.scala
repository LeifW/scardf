package net.croz.scardf

import com.hp.hpl.jena.rdf.model._
import org.joda.time.LocalDate

class Lit( val jLiteral: Literal ) extends Node( jLiteral ) {
  override def rendering: String = {
    if ( isString )
      "\"" + asString + "\"" + ( if (lang.isDefined) {"@" + lang.get.code} else "" )
    else 
      jLiteral.getLexicalForm
  }
  def isString = jLiteral.getValue.getClass == classOf[String]

  val datatype = jLiteral.getDatatype
  val lang: Option[Lang] = {
    val langcode = jLiteral.getLanguage
    if (langcode == "") None else Some( Lang( langcode ) )
  }
  
  override def toString = asString
}

object Lit {
  private val jModel = ModelFactory.createDefaultModel
  private val mapping = scala.collection.mutable.Map[Literal, Lit]()

  def apply( jLiteral: Literal ): Lit = mapping.getOrElseUpdate( jLiteral, new Lit( jLiteral ) )
  def apply( value: String ): Lit = apply( ResourceFactory.createTypedLiteral( value ) )
  def apply( value: String, langCode: String ): Lit = apply( jModel.createLiteral( value, langCode ) )
  def apply( value: String, lang: Lang ): Lit = apply( value, lang.code )
  def apply( value: Int ): Lit = apply( ResourceFactory.createTypedLiteral( value ) )
  def apply( value: Boolean ): Lit = apply( ResourceFactory.createTypedLiteral( value ) )
  def apply( value: Double ): Lit = apply( ResourceFactory.createTypedLiteral( value ) )
  def apply( value: BigDecimal ): Lit = apply( ResourceFactory.createTypedLiteral( value ) )
  def apply( value: LocalDate ): Lit = apply( ResourceFactory.createTypedLiteral( value ) )
  
  def from( o: Any ): Lit = o match {
    case None => null
    case Some( x ) => from( x )
    case str: String => Lit( str )
    case i: Int => Lit( i )
    case b: Boolean => Lit( b )
    case d: Double => Lit( d )
    case bd: BigDecimal => Lit( bd )
    case ld: LocalDate => Lit( ld )
    case x => throw new RdfConversionException( "Cannot create literal from " + x + ": unknown type" )
  }
}

case class RdfConversionException( msg: String ) extends RuntimeException( msg )

case class Lang( val code: String ) {
  def apply( str: String ) = Lit( str, this )
}

object Lang {
  val en = new Lang( "en" )
  val hr = new Lang( "hr" )
}
