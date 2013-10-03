package org.scardf

import org.joda.time.LocalDate
import org.specs2.mutable._
import org.joda.time.LocalDate
import NodeConverter._
import PeopleVoc.{ name => n, _ }
import Doe._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object PtreeSpecs extends Specification {
  val g = new jena.JenaGraph ++= Doe.graph.triples
  "Predicate tree" should {
    "extract deep subgraphs" in {
      val pt = PredicateTree( n~( given, family ), likes?, spouse?~( n~given ) )
      val constructed = g.construct(
        pt.buildPatternBlock( john ).construct
      )
      println( constructed.rend )
      val expectedGraph = new jena.JenaGraph ++ Graph.build(
        john -( n -> Branch( given -> "John", family -> "Doe" ), 
          likes -> swimming, likes -> science,
          spouse -> ( jane-( n -> Branch( given -> "Jane" ) ) )
        )
      )
       ( constructed =~ expectedGraph ) must beTrue
    }
//    "extract subgraphs with collections" in {
//      val rg = new MutableSetGraph()
//      val pt = PredicateTree( n~given, children? )
//      val constructed = g.queryEngineOpt.get.construct(
//        pt.buildPatternBlock( john ).construct
//      )
////      pt.spanRdfList( g/john/children/asGraphNode, rg )
//      val expectedGraph = Graph.build(
//        john -( 
//          n -> Branch( given -> "John", children -> List( anna, bob ) )
//        ),
//        anna -n-> Branch( given -> "Anna" ),
//        bob  -n-> Branch( given -> "Bob" )
//      )
//      println( constructed.rend )
//      constructed mustVerify( _ =~ expectedGraph )
//    }
    "reverse" in {
      val pt = PredicateTree( -AnyPredicate~n~given )
      val query = pt.buildPatternBlock( john ).construct
      val constructed = g.construct(query)
      println( "Const: " + constructed.rend )
      val grown = new jena.JenaGraph ++ pt.growNew(g/john).graph
      println( "Grown: " + grown.rend )
      val expectedGraph = new jena.JenaGraph ++ Graph.build(
        jane -( spouse-> john, n-> Branch( given -> "Jane" ) )
      )
       ( constructed =~ expectedGraph ) must beTrue
      ( grown =~ expectedGraph ) must beTrue
    }
    "all S and O triples" in {
      val pt = PredicateTree( AnyPredicate?, -AnyPredicate? )
      println( pt )
      val f = anna
      val constructed = g.construct( pt.buildPatternBlock( f ).construct )
      val grown = new jena.JenaGraph ++ pt.growNew(g/f).graph
      val expectedGraph = new jena.JenaGraph ++ g.triplesLike( f, Node, Node ) ++ g.triplesLike( Node, Node, f )
      ( constructed =~ expectedGraph ) must beTrue
      ( grown =~ expectedGraph ) must beTrue
    }
  }
}