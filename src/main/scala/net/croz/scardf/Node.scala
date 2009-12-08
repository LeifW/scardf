package net.croz.scardf

import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.RDFList
import com.hp.hpl.jena.rdf.model.Literal
import org.joda.time.LocalDate
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat.date
import org.joda.time.format.ISODateTimeFormat.dateTime

class Node( val jNode: RDFNode ) {

  def /( p: Prop ): NodeBag = asRes/p
  def /( pp: PropPath ): NodeBag = asRes/pp
  def /[T]( converter: NodeConverter[T] ): T = converter( this )

  def isUriResource = jNode.isURIResource
  def isRes = jNode.isResource
  def isBlank = jNode.isAnon
  def isRdfList = jNode.canAs( classOf[RDFList] )
  def isLit = jNode.isLiteral
  def isLitOn( lang: Lang ) = 
    if ( isLit ) asLiteral.getLanguage == lang.code 
    else false
  
  def rendering: String = {
    if ( jNode.isLiteral ) asLit.rendering
    else if ( jNode.isResource ) asRes.rendering
    else "?node?"
  }
  
  def lexic = 
    if ( jNode.canAs( classOf[String] ) ) asString
    else rendering

  private def asLiteral: Literal = jNode.as( classOf[Literal] ).asInstanceOf[Literal]

  def asRes: Res = Res( jNode.as( classOf[Resource] ).asInstanceOf[Resource] )
  def asProp: Prop = Prop( jNode.as( classOf[Property] ).asInstanceOf[Property] )
  def asLit = Lit( asLiteral )
  def asRdfList: RdfList = RdfList from jNode.as( classOf[RDFList] ).asInstanceOf[RDFList]
  def asString: String = asLiteral.getString
  def asBoolean: Boolean = asLiteral.getBoolean
  def asInt: Int = asLiteral.getInt
  def asDouble: Double = asLiteral.getDouble
  def asBigDecimal: BigDecimal = BigDecimal( asLiteral.getLexicalForm )
  def asLocalDate: LocalDate = date.parseDateTime( asLiteral.getLexicalForm ).toLocalDate
  def asDateTime: DateTime = dateTime.parseDateTime( asLiteral.getLexicalForm ).toDateTime
  
  override def equals( o: Any ) = o match {
    case that: Node => this.jNode.asNode.sameValueAs( that.jNode.asNode )
    case _ => false
  }
  override def hashCode = jNode.hashCode
  override def toString = rendering
}

object Node {

  def apply( jNode: RDFNode ) = wrap( jNode )
  
  def from( o: Any ): Node = o match {
    case n: Node => n
    case n: RDFNode => wrap( n )
    case x => Lit from x
  }
  
  private def wrap( jNode: RDFNode ): Node = jNode match {
    case null        => null
    case rl: RDFList => RdfList from rl
    case p: Property => Prop( p )
    case r: Resource => Res( r )
    case l: Literal  => Lit( l )
    case n => throw new RuntimeException( "Unknown type of RDFNode: " + n )
  }
  
  /**
   * Sorts a list of nodes by its rendering.
   */
  def sort( list: List[Node] ): List[Node] = list sort { (a, b) => a.rendering < b.rendering }
}
