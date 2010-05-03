package org.scardf

import org.joda.time.LocalDate
import org.specs._
import org.specs.runner.JUnit4
import NodeConverter._

object PrimerSpecs extends Specification {
  "NodeBag" should {
    import PeopleVoc._
    import Doe._
    "" in {
      val g = Graph.build( john -(
        PeopleVoc.name -> Branch( given -> "John", family -> "Doe" ),
        spouse -> ( jane -spouse -> john )
      ) )
      g
    }
    "throw exception on taking one node from empty bag" in {
      Graph().bagOf().singleNode must throwA[ Exception ]
    }
    "do equals" in {
      val g = Graph()
      g.bagOf( "a" ) must_== g.bagOf( "a" )
      g.bagOf( "a" ) must_!= g.bagOf( "a", "b" )
      g.bagOf( "b", "a" ) must_== g.bagOf( "a", "b" )
      g.bagOf( "b", "a" ) must_!= g.bagOf( "b", "a", "b" )
    }
    "flow" in {
      println( 
        new jena.JenaGraph() ++ Doe.graph 
        renderIn Turtle prefixes( 'p -> PeopleVoc, 'd -> Doe, 'rdf -> RDF, 'xsd -> XSD ) asString
      )
      val g = Doe.graph
      g/john/PeopleVoc.name/given/asString must_== "John"
      g/john/PeopleVoc.name/given.v must_== "John"
      g/john/isMale/asBoolean must_== true
      g/john/height/asInt must_== 167
      g/john/birthday/asLocalDate must_== new LocalDate( 1977, 7, 27 )
      ( g/anna/spouse ).isEmpty must_== true
      g/anna/weight/asInt.default( 100 ) must_== 100
      for ( r <- g/john/likes ) println( r )
      g/john/likes/asNode.set must_== Set( swimming, science )
      ( g/john/isMale? ) must_== true
      ( g/anna/spouse? ) must_== false
      g/john has height -> 167 must_== true
      (g/john has( likes -> science ));
    }
    "using the where and having clause" in {
      val g = graph
      val familyMembers = g/-/having( RDF.Type -> person )
      "filter using node methods" in {
        g.bagOf( 1, john, "a" )/where( _.node.isLiteral ) must_== g.bagOf( 1, "a" )
      }
      "filter using path expressions" in {
        familyMembers must_== g.bagOf( john, jane, anna, bob )
        familyMembers/where( _/isMale? ) must_== g.bagOf( john, bob )
        familyMembers/where( _/spouse/isMale? )/asNode.set must_== Set( jane )
        familyMembers/where( _/likes contains science ) must_== g.bagOf( john )
      }
      "filter with missing assignments" in {
        g/john has( weight -> None ) must_== true
        familyMembers/having( weight -> None ) must_== familyMembers
      }
    }
  }
}
