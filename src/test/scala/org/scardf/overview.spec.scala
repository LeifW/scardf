package org.scardf

import org.joda.time.LocalDate
import NodeConverter._
import org.scalatest._
import scala.language.postfixOps

class PrimerSpecs extends WordSpec with ShouldMatchers {
  import PeopleVoc._
  import Doe._
  val g = Graph(john -(
      PeopleVoc.name -> Branch( given -> "John", family -> "Doe" ),
      spouse -> ( jane -spouse -> john )
    ))

  "NodeBag" should {
    "throw exception on taking one node from empty bag" in {
      an[Exception] should be thrownBy Graph().bagOf().singleNode
    }

    "do equals" in {
      val g = Graph()
      g.bagOf("a") should be(g.bagOf("a"))
      g.bagOf("a") should not be (g.bagOf("a", "b"))
      g.bagOf("b", "a") should be(g.bagOf("a", "b"))
      g.bagOf("b", "a") should not be (g.bagOf("b", "a", "b"))
    }

    "flow" in {
      val g = Doe.graph.asInstanceOf[SetGraph]
      g / john / PeopleVoc.name / given / asString should be("John")
      g / john / PeopleVoc.name / given.v should be("John")
      g / john / isMale / asBoolean should be(true)
      g / john / height / asInt should be(167)
      g / john / birthday / asLocalDate should be(new LocalDate(1977, 7, 27))
      (g / anna / spouse).isEmpty should be(true)
      g / anna / weight / asInt.default(100) should be(100)
      for (r <- g / john / likes) println(r)
      g / john / likes / asNode.set should be(Set(swimming, science))
      (g / john / isMale ?) should be (true)
      (g / anna / spouse ?) should be (false)
      g / john has height -> 167 should be(true)
      (g / john has (likes -> science));
    }
  }

  "using the where and having clause" should {
    val g = graph
    val familyMembers = g/-/having( RDF.Type -> person )
    "filter using node methods" in {
      g.bagOf( 1, john, "a" )/where( _.node.isLiteral ) should be (g.bagOf( 1, "a" ))
    }

    "filter using path expressions" in {
      familyMembers should be (g.bagOf( john, jane, anna, bob ))
      familyMembers/where( _/isMale? ) should be (g.bagOf( john, bob ))
      familyMembers/where( _/spouse/isMale? )/asNode.set should be (Set( jane ))
      familyMembers/where( _/likes contains science ) should be (g.bagOf( john ))
    }

    "filter with missing assignments" in {
      g/john has( weight -> None ) should be (true)
      familyMembers/having( weight -> None ) should be (familyMembers)
    }
  }

  "triple matching" should {
    val g = Doe.graph
    "pattern matching" in {
      g.triples filter { _ match {
        case RdfTriple( `anna`, `height`, _ ) => true
        case _ => false
      } }
      g.triplesMatching {
        case RdfTriple( `anna`, `height`, _ ) => true
      }.toList should be (List(RdfTriple( anna, height, Node from 107)))
      g.triplesMatching {
        case RdfTriple( _, `height`, h: Literal ) => asInt(h) < 100
      }.map{ _.subj }.toList should be (List( bob ))
    }
    "triplesLike with Node placeholder" in {
      g.triplesLike( anna, height, Node ).toList.size should be (1)
      g.triplesLike( anna, height, Node ).toList should be (List( RdfTriple( anna, height, Node from 107 )))
    }
    "triplesLike with a closure" in {
      g.triplesLike( Node, height, { h: Literal => asInt(h) < 100 } ).map{ _.subj }.toList should be (List( bob ))
    }
  }

  "QueryEngineBackedGraph over Jena" should {
    import PeopleVoc._
    import Doe._

    class DemoQEBGraph( jg: jena.JenaGraph ) extends QueryEngineBackedGraph {
      override def select( q: String ): List[Map[QVar, Node]] = { println( q ); jg.select(q) }
      override def ask( q: String ): Boolean = { println( q );  jg.ask(q) }
    }

    val jg = new jena.JenaGraph
    jg ++= Doe.graph
    val g = new DemoQEBGraph( jg )

    "delegate triplesLike" in {
      g.triplesLike( SubjectNode, Some(height), TypedLiteral ).map{ _.subj }.toSet should be (Set( anna, bob, john, jane ))
      g.triplesLike( SubjectNode, height, 99 ).map{ _.subj }.toList should be (List( bob ))
    }
    "delegate contains" in {
      g.contains( RdfTriple( bob, height, Node from 99 ) ) should be (true)
      g.contains( RdfTriple( john, height, Node from 99 ) ) should be (false)
    }
  }
}
