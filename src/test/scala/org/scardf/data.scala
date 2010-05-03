package org.scardf

import NodeConverter._

object PeopleVoc extends Vocabulary( "http://person.eg#" ) {
  val person = ÷( "Person" )
  val hobby = ÷( "Hobby" )
  val swimming = prop( "Swimming" )
  val science = prop( "Science" )
  val name = prop( "name" )
  val given = propStr( "given" )
  val family = propStr( "family" )
  val isMale = ÷( "isMale" )
  val height = propInt( "height" )
  val weight = propInt( "weight" )
  val likes = prop( "likes" )
  val spouse = prop( "spouse" )
  val children = ÷( "children" )
  val birthday = ÷( "birthday" )
  val age = propInt( "age" )
}

import PeopleVoc._

object Doe extends Vocabulary( "http://doe.eg#" ) {
  val List( anna, bob, john, jane ) = List( 'anna, 'bob, 'john, 'jane ) map{ this÷_ }

  // shortcut pairs for describing type and gender
  private val aPerson = RDF.Type -> person
  private val aMale = isMale -> true
  private val aFemale = isMale -> false

  private val basegraph = Graph.build(
    anna -( aPerson, aFemale, 
      name -> Branch( given -> "Anna" ), height -> 107, birthday -> "2004-04-14",
      likes -> swimming
    ),
    bob -( aPerson, aMale, 
      name -> Branch( given -> "Bob" ), height -> 99, birthday -> "2007-05-18"
    ),
    john -( aPerson, aMale, 
      name -> Branch( given -> "John" ), height -> 167, birthday -> "1977-07-27",
      likes -> ObjSet( swimming, science ), children -> List( anna, bob ), spouse -> jane
    ),
    jane -( aPerson, aFemale, 
      name -> Branch( given -> "Jane" ), height -> 150, birthday -> "1976-06-26",
      likes -> swimming, children -> List( anna, bob ), spouse -> john
    )
  )
  
  // triples specifying last name "Doe" for every person in the basegraph are constructed
  val graph0 = basegraph ++ 
    ( basegraph/-/name/asSubjectNode.iterable map { sn => Triple( sn, family, Literal( "Doe" ) ) } )

//  val metersHigh = ÷( "metersHigh" )
//  val graph = Augment triples {
//    case Triple( s, `height`, TypedLiteral( hInCm, _ ) ) => List( s -metersHigh-> ( hInCm.toInt/100 ) )
//  } augmented basegraph
  
  val graph = Augment add { _ -family-> "Doe" } forEach { _/-/name } on basegraph
//  val graph = Augment add { p => p -age-> yearsSince( p/birthday/asLocalDate ) } forEach { _/-/having( RDF.Type -> person ) } on basegraph
  
  val familyMembers = graph.bagOf( john, jane, anna, bob )
  
  private def yearsSince( ld: org.joda.time.LocalDate ) = 2010 - ld.getYear
}
