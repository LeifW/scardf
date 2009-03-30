package net.croz.scardf

object QVar {
  //TODO synchronize counter!
  private var last = 0
  def next = { last += 1; last }
  def apply( name: String ) = new NamedQVar( name )
}

/**
 * An object representing a variable in queries.
 * Two variables are equal if their names are equal.
 */
class QVar {
  val num = QVar.next
  def name = "v" + num

  override def equals( o: Any ) = o match {
    case that: QVar => this.name == that.name
    case _ => false
  }
  override def hashCode = name.hashCode

  override val toString = "?" + name
}

class NamedQVar( override val name: String ) extends QVar

object ResX extends NamedQVar( "ResX" )
