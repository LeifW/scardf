package net.croz.scardf

import org.joda.time.LocalDate
import org.specs2.mutable.Specification

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object NoVarSpec extends Specification {

  "j" should {
    import PeopleVocabulary._
    "" in {
      val family = new Vocabulary( "http://voc.eg#" )
      Spouse( family\"John" ) = family~"Jane"
      (family~"Mother")( family\"Jane" ) = family~"Vilma"
      val MotherInLaw = Spouse ~ (family~"Mother")
      family\"John"/MotherInLaw/asRes must_== family~"Vilma"
    }
    "" in {
      val family = new Vocabulary( "http://voc.eg#" )
      val List( john, jane, vilma, anna, bob ) = 
        List( "John", "Jane", "Vilma", "Anna", "Bob" ) map{ family\_ }
      john.uri must_== "http://voc.eg#John"
    }
  }
}
