package org.scardf.jena

import org.scardf._
import scala.collection.mutable.ArrayBuffer
import com.hp.hpl.jena.rdf.model.{Literal => JLiteral, Property => JProperty, _}
import com.hp.hpl.jena.sparql.core.describe._
import com.hp.hpl.jena.sparql.util.Context

class PredTreeDescribeHandler( pt: PredicateTree ) extends DescribeHandler {
  var resultGraph: MutableGraph = null;
  
  def start( accumulateResultModel: Model, qContext: Context ) {
    resultGraph = new JenaGraph( accumulateResultModel )
  }
  
  def describe( r: Resource ) {
    pt.grow( Jena graphNode r, resultGraph )
  }
  
  def finish() {}
}

class PredTreeDescribeHandlerFactory( pt: PredicateTree ) extends DescribeHandlerFactory {
  def create() = new PredTreeDescribeHandler( pt )
}

class PtTypeMapDescribeHandler( pttMap: Map[UriRef, PredicateTree] ) extends DescribeHandler {
  var resultGraph: MutableGraph = null;
  
  def start( accumulateResultModel: Model, qContext: Context ) {
    resultGraph = new JenaGraph( accumulateResultModel )
  }
  
  def describe( r: Resource ) {
    val gn = Jena graphNode r
    for ( rtype <- gn/RDF.Type/NodeConverter.asUriRef.set; pt <- pttMap get rtype ) {
      println( "Entered DESCRIBE " + pt + " with " + gn )
      pt.grow( gn, resultGraph )
    }
  }
  
  def finish() {}
}
class PtTypeMapDescribeHandlerFactory( pttMap: Map[UriRef, PredicateTree] ) extends DescribeHandlerFactory {
  def create() = new PtTypeMapDescribeHandler( pttMap )
}
