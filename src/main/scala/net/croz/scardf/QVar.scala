package net.croz.scardf

object QVar {
  //TODO synchronize counter!
  private var last = 0
  def next = { last += 1; last }
  
  def apply() = new NumQVar
  def apply( name: String ) = new QVar( name )
  
  def rendering( n: String ) = "?" + n
  
  implicit def toQVar( s: Symbol ) = new QVar( s.name )
}

/**
 * An object representing a variable in queries.
 * Two variables are equal if their names are equal.
 */
class QVar( val name: String ) {

  override def equals( o: Any ) = o match {
    case that: QVar => this.name == that.name
    case _ => false
  }

  override def hashCode = name.hashCode

  override val toString = "?" + name
}

class NumQVar extends QVar( "v" + QVar.next )

object X extends QVar( "X" )
