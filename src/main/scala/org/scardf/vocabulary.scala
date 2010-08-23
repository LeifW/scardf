package org.scardf

class Vocabulary( val prefix: String ) {
  def \( suffix: String ) = UriRef( prefix + suffix )
  def ÷( suffix: String ) = UriRef( prefix + suffix )
  def ÷( suffixSymbol: Symbol ): UriRef = ÷( suffixSymbol.name )
  
  import NodeConverter._
  def prop( suffix: String ) = GNProperty( prefix + suffix )
  def propInt( suffix: String ) = Property[Int]( prefix + suffix )
  def propStr( suffix: String ) = Property[String]( prefix + suffix )( asString )
}

case object XSD extends Vocabulary( "http://www.w3.org/2001/XMLSchema#" ) {
  val string = XSD÷"string"
  val integer = XSD÷"integer"
  val long = XSD÷"long"
  val boolean = XSD÷"boolean"
  val decimal = XSD÷"decimal"
  val float = XSD÷"float"
  val double = XSD÷"double"
  val date = XSD÷"date"
  val time = XSD÷"time"
  val dateTime = XSD÷"dateTime"
  val duration = XSD÷"duration"
}

case object RDF extends Vocabulary( "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ) {
  val Type = prop( "type" )
  val ID = RDF÷"ID"
  val nil = RDF÷"nil"
  val first = RDF÷"first"
  val rest = RDF÷"rest"
}
