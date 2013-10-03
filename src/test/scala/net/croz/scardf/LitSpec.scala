package net.croz.scardf

import org.joda.time.LocalDate
import org.specs2.mutable.Specification

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object LitSpec extends Specification {
  "Literals" should {
    "be created from String" in {
      Lit( "example" ).rendering must_== "\"example\""
    }
    "be created from String with Lang" in {
      Lit( "example", Lang.en ).rendering must_== "\"example\"@en"
    }
    "be created from Boolean" in {
      Lit( true ).rendering must_== "true"
    }
    "be created from Int" in {
      Lit( 1 ).rendering must_== "1"
      Lit( 1 ).asInt must_== 1
    }
    "be created from Double" in {
      Lit( 1.1D ).rendering must_== "1.1"
      Lit( 1.1D ).asDouble must_== 1.1D
    }
    "be created from BigDecimal" in {
      val digits = "-1.2345678901234567890123"
      Lit( BigDecimal( digits ) ).rendering must_== digits
      Lit( BigDecimal( digits ) ).asBigDecimal must_== BigDecimal( digits )
    }
    "be created from LocalDate" in {
      Lit( new LocalDate( 2007, 7, 7 ) ).rendering must_== "2007-07-07"
      Lit( new LocalDate( 2007, 7, 7 ) ).asLocalDate must_== new LocalDate( 2007, 7, 7 )
    }
    "do equals" in {
      Lit( "a" ) must_== Lit( "a" )
    }
  }
}
