package net.croz.scardf

import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.vocabulary.{RDF => jRDF}
import com.hp.hpl.jena.vocabulary.{RDFS => jRDFS}
import com.hp.hpl.jena.vocabulary.{XSD => jXSD}

class Vocabulary( val prefix: String ) {
  val model = new Model withPrefix prefix
  
  def apply( name: String ) = pRes( name )
  def *( name: String ) = pRes( name )
  def \( name: String ) = pRes( name )
  def ~( name: String ) = pProp( name )  
  def §( name: String ) = pProp( name )  
  def pRes( name: String ) = Res( name )( model )
  def pProp( name: String ) = Prop( name )( model )
  def wRes( r: Resource ) = Res( r, model )
  def wProp( p: Property ) = Prop( p, model )
}

object RDF extends Vocabulary( jRDF.getURI ) {
  val Type = wProp( jRDF.`type` )
  val first = wRes( jRDF.first )
  val rest = wRes( jRDF.rest )
}

object RDFS extends Vocabulary( jRDFS.getURI ) {
  val range = wProp( jRDFS.range )
  val Datatype = wRes( jRDFS.Datatype )
}

object XSD extends Vocabulary( jXSD.getURI ) {
  val string = wRes( jXSD.xstring )
  val boolean = wRes( jXSD.xboolean )
  val int = wRes( jXSD.xint )
  val date = wRes( jXSD.date )
  val dateTime = wRes( jXSD.dateTime )
}