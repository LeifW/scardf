package net.croz.scardf

import org.joda.time.LocalDate
import org.specs._
import org.specs.runner.JUnit4

class QuerySpecsTest extends JUnit4(QuerySpecs)

object QuerySpecs extends Specification {
  "Query mechanism" should {
    implicit val data = new Model() withPrefix "http://family.eg#"
    import PeopleVocabulary._
    val aMale = IsMale -> true
    val aFemale = IsMale -> false
    val anna = Res( "anna" ) a Person state(
      Name -> Anon( Given -> "Anna" ),
      aFemale, Birthday -> "2004-04-14", Height -> 107,
      Likes -> Swimming
    )
    val bob = Res( "bob" ) a Person state(
      Name -> Anon( Given -> "Bob" ),
      aMale, Birthday -> "2007-05-18", Height -> 87
    )
    val jdoe = Res( "jdoe" ) a Person state(
      Name -> Anon( Given -> "John" ),
      aMale, Birthday -> "1977-07-27", Height -> 167,
      Likes -> All( Swimming, Science ),
      Children -> RdfList( anna, bob ), Spouse -> Res( "jane" )
    )
    val jane = Res( "jane" ) a Person state(
      Name -> Anon( Given -> "Jane" ),
      aFemale, Birthday -> "1976-06-26", Height -> 150,
      Likes -> Swimming,
      Children -> RdfList( anna, bob ), Spouse -> jdoe
    )
    for ( p <- List( anna, bob, jane, jdoe ) )
      p/Name/asRes state Family -> "Doe"
    
    "select, order, offset and limit" in {
      val person, height = new QVar
      val selectPersonsByHeight = ( Sparql 
        select( person, height ) 
          where( (person, RDF.Type, Person), (person, Height, height) )
          orderBy( asc( height ) )
          limit 2 offset 1
      )
      ( selectPersonsByHeight from data ).solutions == List(
        Map( person -> anna, height -> Lit(107) ), Map( person -> jane, height -> Lit(150) )
      )
    }
    "select with/without DISTINCT" in {
      val person, hobby = new QVar
      val distinctHobbies = Sparql select distinct( hobby ) where( (person, Likes, hobby) ) from data
      distinctHobbies.solutions.map{ _(hobby) } must_== List( Swimming, Science )
      val allHobbiesResult = Sparql select hobby where( (person, Likes, hobby) ) from data
      val hobbies = allHobbiesResult.solutions.map{ _(hobby).asRes }
      hobbies.size must beGreaterThanOrEqualTo( 2 )
      hobbies must containAll( Set( Swimming, Science ) )
    }
    "select one X as option" in {
      Sparql selectX asRes where( (X, Likes, Science) ) from data must_== Some( jdoe )
      Sparql selectX asInt where( (jane, Height, X) ) from data must_== Some( 150 )
      Sparql selectX asInt where( (jane, Weight, X) ) from data must_== None
    }
    "select all X as iterator" in {
      val iter = Sparql selectAllX asRes where( (X, Likes, Swimming) ) from data
      Set.empty ++ iter.toList must_== Set( anna, jane, jdoe )
    }
  }
}
