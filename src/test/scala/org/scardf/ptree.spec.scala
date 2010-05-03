package org.scardf

import org.joda.time.LocalDate
import org.specs._
import org.specs.runner.JUnit4
import org.joda.time.LocalDate
import NodeConverter._
import PeopleVoc.{ name => n, _ }
import Doe._

object PtreeSpecs extends Specification {
  val g = Doe.graph
  "Predicate tree" should {
    "extract deep subgraphs" in {
      val pt = PredicateTree( n~( given, family ), likes?, spouse?~( n~given ) )
      val xJohn = pt.growNew( g/john )
      val expectedGraph = Graph.build(
        john -( n -> Branch( given -> "John", family -> "Doe" ), 
          likes -> swimming, likes -> science,
          spouse -> ( jane-( n -> Branch( given -> "Jane" ) ) )
        )
      )
      xJohn.node must_== john
      xJohn.graph mustVerify( _ =~ expectedGraph )
      println( pt.buildPatternBlock( john ).construct )
    }
    "extract subgraphs with collections" in {
      val rg = new MutableSetGraph()
      val pt = PredicateTree( n~given, children? )
      val xJohn = pt.grow( g/john, rg )
      pt.spanRdfList( g/john/children/asGraphNode, rg )
      val expectedGraph = Graph.build(
        john -( 
          n -> Branch( given -> "John", children -> List( anna, bob ) )
        ),
        anna -n-> Branch( given -> "Anna" ),
        bob  -n-> Branch( given -> "Bob" )
      )
      println( rg.rend )
      xJohn.node must_== john
      xJohn.graph mustVerify( _ =~ expectedGraph )
    }
  }
}
