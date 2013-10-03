package org.scardf

case class Vocabulary( prefix: String ) {
  def uriref( suffix: String ) = UriRef( prefix + suffix )
  def \( suffix: String ) = uriref( suffix )
  def ÷( suffix: String ) = uriref( suffix )
  def ÷( suffixSymbol: Symbol ): UriRef = ÷( suffixSymbol.name )
  
  import NodeConverter._
  def prop( suffix: String ) = GNProperty( prefix + suffix )
  def propInt( suffix: String ) = Property[Int]( prefix + suffix )
  def propStr( suffix: String ) = Property[String]( prefix + suffix )( asString )
}

object XSD extends Vocabulary( "http://www.w3.org/2001/XMLSchema#" ) {
  val string = XSD\"string"
  val integer = XSD\"integer"
  val int = XSD\"int"
  val long = XSD\"long"
  val boolean = XSD\"boolean"
  val decimal = XSD\"decimal"
  val float = XSD\"float"
  val double = XSD\"double"
  val date = XSD\"date"
  val time = XSD\"time"
  val dateTime = XSD\"dateTime"
  val duration = XSD\"duration"
}

object RDF extends Vocabulary( "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ) {
  val Type = prop( "type" )
  val ID = RDF\"ID"
  val nil = RDF\"nil"
  val first = RDF\"first"
  val rest = RDF\"rest"
  val XMLLiteral = RDF\"XMLLiteral"
  val Property = RDF\"Property"
  val Statement = RDF\"Statement"
  val Bag = RDF\"Bag"
  val Seq = RDF\"Seq"
  val Alt = RDF\"Alt"
  val List = RDF\"List"
  val value = RDF\"value"
  val subject = RDF\"subject"
  val predicate = RDF\"predicate"
  val `object` = RDF\"object"
}

object RDFS extends Vocabulary( "http://www.w3.org/2000/01/rdf-schema#" ) {
  val Resource = RDFS\"Resource"
  val Literal = RDFS\"Literal"
  val Class = RDFS\"Class"
  val Datatype = RDFS\"Datatype"
  val Container = RDFS\"Container"
  val ContainerMembershipProperty = RDFS\"ContainerMembershipProperty"
  val subClassOf = prop("subClassOf")
  val subPropertyOf = prop("subPropertyOf")
  val domain = prop("domain")
  val range = prop("range")
  val label = propStr( "label" )
  val comment = propStr( "comment" )
  val member = RDFS\"member"
  val seeAlso = RDFS\"seeAlso"
  val isDefinedBy = RDFS\"isDefinedBy"
}
