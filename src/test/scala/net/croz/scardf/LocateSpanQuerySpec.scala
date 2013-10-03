package net.croz.scardf

import PeopleVocabulary._
import FamilyVocabulary._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.collection.mutable.{ ListBuffer, Map => MMap }
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
object LocateSpanQuerySpec extends Specification with specs.RdfMatchers {

  "query system" should {
    val template = Blank( Likes -> Swimming ).root
    val ptree = PredicateTree( Name~(Given, Family), Height )
    
    "select some" in {
      val qLikes = QVar()
      val selectQ = Sparql select( X, qLikes ) where( (X, IsMale, true), (X, Likes, qLikes) )
      println( selectQ from FamilyVocabulary.model solutions )
      true must beTrue
    }
    
//    "select using locate-span" in {
//      val q = new LocateSpanQ( template, ptree, List( Height -> false ) )
//      val result = q from FamilyVocabulary.model
//      val rm = result.model
//      result must_==( RdfList( john, jane, anna )( rm ) )
//    }
  }
}

class LocateSpanQ(
  template: Res, 
  ptree: PredicateTree, 
  orderCriteria: List[ Pair[PredicateChain, Boolean] ] 
) 
{
  private val qvars = MMap[PredicateChain, QVar]( PropPath() -> X )
  private val constraints = new ListBuffer[Tuple3[Any,Any,Any]]
  private val comparators = new ListBuffer[OrderComparator]
  private var lim = 0
  private var off = 0
  
  def selectQuery = (
    new query.SelectQ( qvars.values.toList: _* ) where( constraints: _* )
    orderBy( comparators: _* ) limit lim offset off
  )
  
  def toRdfList( solutionIt: Iterator[QSolution] ) = {
    var m: Model = null
    for ( qsolution <- solutionIt ) {
      val solution = qsolution.toMap
      val a = solution( X ).asRes
      ptree.growFrom( a )
      for ( chain <- qvars.keySet )
        chain( a ) = solution( qvars( chain ) )
    }
    RdfList()(m)
  }
  
  def from( dataModel: Model ) = toRdfList( selectQuery from dataModel ) 
}
//ORDER BY ?H

