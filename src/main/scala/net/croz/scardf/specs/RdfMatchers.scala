package net.croz.scardf.specs

import net.croz.scardf.Model
import org.specs.matcher.Matcher

trait RdfMatchers {

  def be_=~( m1: Model ) = beIsomorphicWith( m1 )

  def beIsomorphicWith( m1: Model ) = new Matcher[Model] {
    def apply( m2: => Model ) = ( 
      m1.local =~ m2.local, 
      m1 + " is isomorphic with " + m2, 
      m1 + " is not isomorphic with " + m2 
    )
  }
}
