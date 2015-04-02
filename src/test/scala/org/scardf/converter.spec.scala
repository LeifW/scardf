package org.scardf

import org.joda.time.LocalDate
import NodeConverter._
import PeopleVoc._
import Doe._
import org.scalatest._

class ConverterSpecs extends WordSpec with ShouldMatchers {
  val g = Doe.graph
  "value converters" should {
    "convert typed literal to Scala value" in {
      TypedLiteral( "10", XSD.integer )/asInt should be (10)
    }
    "convert plain literal to typed Scala value" in {
      PlainLiteral( "10" )/asInt should be (10)
    }
    "convert bags to single value" in {
      g/john/spouse/height/asInt should be (150)
      g/john/birthday/asLocalDate should be (new LocalDate( 1977, 7, 27 ))
      g/john/birthday/asString should be ("1977-07-27")
    }
  }
  "predicate-converters" should {
    "convert subject node to bag" in {
      g/john/height should be (NodeBag( List( TypedLiteral( "167", XSD.int ) ), g ))
      g/john/likes should be (g.bagOf( swimming, science ))
      g/bob/likes should be (g.bagOf())
    }
    ".option" in {
      g/bob/likes.option should be (None)
    }
    "with .n modifier, get node" in {
      g/anna/likes.n should be (swimming)
    }
    "with .v modifier, convert node to value" in {
      g/john/height.v should be (167)
    }
    "list all values for fixed predicate" in {
      g/-/height should be (g.bagOf( 99, 107, 150, 167 ))
    }
    "list all values for fixed predicate, multiple values" in {
      g/-/likes should be (g.bagOf( swimming, swimming, swimming, science))
    }
    "distinct filter" in {
      g/-/likes/distinct should be (g.bagOf( swimming, science ))
    }
    "with .set modifier, convert bag to Set" in {
      g/-/height.set should be (Set( 99, 107, 150, 167 ))
    }
    "pred set converter" in {
      familyMembers/likes.set should be (Set( swimming(g), science(g)))
    }
  }
}
