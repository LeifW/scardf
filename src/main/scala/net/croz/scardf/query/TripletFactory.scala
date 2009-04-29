package net.croz.scardf.query

import net.croz.scardf._

object TripletFactory {

  def tripletsFrom( m: Model ) = {
    val tf = new TripletFactory
    m.local.statements map{ tf toTriplet _ } toList
  }
}

private class TripletFactory {
  val varMap = scala.collection.mutable.Map[ Res, QVar ]()
  
  def replaced( o: Any ) = o match {
    case n: Res if n.isBlank => varMap.getOrElseUpdate( n, QVar() )
    case other => other
  }
  
  def toTriplet( stmt: Stmt ) = ( replaced( stmt.s ), stmt.p, replaced( stmt.o ) )
}
