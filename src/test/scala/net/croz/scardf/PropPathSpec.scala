package net.croz.scardf

import org.specs2.mutable.Specification
import PeopleVocabulary._
import FamilyVocabulary._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object PropPathSpec extends Specification with specs.RdfMatchers {
  "normal prop path" should {
    val pp = PropPath( Name, Given )
    "be constructed using dashes" in {
      val longPath = Spouse~Likes~Name~Given
      longPath.toList must_== List( Spouse, Likes, Name, Given )
    }
    "traverse graphs using slash operator" in {
      anna/pp/asString must_== "Anna"
    }
    "traverse graphs using 'of'" in {
      Spouse~Likes of john must_== Swimming
    }
    "do equals" in {
      Name~Given must_== pp
    }
  }
  "emtpy prop path" should {
    val emptyPath = PropPath()
    "traverse graph" in {
      anna/emptyPath must_== NodeBag( anna )
    }
  } 
  "dead-end paths" should { "yield empty node bags" in {
    john/( Spouse~Likes~Name~Given ) must_== NodeBag()
  } }
  "predicate tree" should { 
    import net.croz.scardf.{PredicateTree => pt}
    "be constructed with fork operators" in {
      Spouse~( Name~Given, IsMale ) must_== pt( Spouse -> pt( Name -> pt(Given), IsMale -> pt() ) )
    }
    "grow graphs" in {
      val ptree = Spouse~( Name~Given, IsMale )
      val extRoot = ptree growFrom john
      val m = extRoot.model
      m must_!= FamilyVocabulary.model
      val tm = new Model
      val a = tm.getAnon
      tm addAll List( 
        john( Spouse -> jane ), 
        jane( IsMale -> false ), 
        jane( Name -> a ),
        a( Given -> "Jane" )
      )
      m must be_=~( tm )
    }
    "grow a template graph" in {
      val ptree = Spouse~( Name~Given, IsMale )
      ptree.growTemplate.model must be_=~( 
        Blank( Spouse -> Blank( IsMale -> Blank(), Name -> Blank( Given -> Blank() ) ) ).toModel
      )
    }
    "merge trivial trees" in {
      val t1 = pt( Spouse )
      val t2 = pt( IsMale )
      t1++t2 must_== pt( Spouse, IsMale )
    }
    "merge chains" in {
      val t1: pt = Spouse~Name~Given
      val t2 = Spouse~IsMale
      t1++t2 must_== Spouse~( Name~Given, IsMale )
    }
    "merge a tree to itself" in {
      val t = Spouse~( Name~Given, IsMale )
      t++t must_== t
    }
    "merge three trees" in {
      val t1 = Spouse~( Name~Given, IsMale )
      val t2 = pt( Likes )
      val t3 = pt( IsMale, Spouse~( IsMale, Name~Family ) )
      t1++t2++t3 must_== pt( IsMale, Likes, Spouse~( Name~(Given, Family), IsMale ) )
      t3++t1++t2 must_== pt( IsMale, Likes, Spouse~( Name~(Given, Family), IsMale ) )
    }
    "remove trivial tree from itself" in {
      pt( IsMale ) -- pt( IsMale ) must_== pt.empty
    }
    "remove tree" in {
      Spouse~( Name~Given, IsMale ) -- ( Spouse~Name~(Given, Family) ) must_== Spouse~(Name, IsMale)
      Spouse~( Name~Given, IsMale ) -- ( Spouse~Name ) must_== Spouse~( Name~Given, IsMale )
    }
    "subgraph from chain" in {
      val t = Spouse~( Name~Given, IsMale )
      val p = Spouse~Name
      t subtree p must_== pt( Given )
    }
  }
}
