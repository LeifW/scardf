package net.croz.scardf.specs

import net.croz.scardf.Model
import org.specs2.matcher.Matcher
import org.specs2.matcher.Expectable
import org.specs2.matcher.MatchResult
import org.specs2.matcher.MatchSuccess

trait RdfMatchers {

  def be_=~(m1: Model) = beIsomorphicWith(m1)

  def beIsomorphicWith(m1: Model) = new BeIsomorphicMatcher(m1)

  class BeIsomorphicMatcher(m1: Model) extends Matcher[Model] {
    def apply[S <: Model](exm2: Expectable[S]): MatchResult[S] = {
      result(
        m1.local =~ exm2.value.local,
        m1 + " is isomorphic with " + exm2.value,
        m1 + " is not isomorphic with " + exm2.value,
        exm2)
    }
  }

  /* def beIsomorphicWith( m1: Model ) = new Matcher[Model] {
    def apply( m2: => Model ) = ( 
      m1.local =~ m2.local, 
      m1 + " is isomorphic with " + m2, 
      m1 + " is not isomorphic with " + m2 
    )
  }*/
}
