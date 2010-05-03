package org.scardf

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
class QVar( val name: String ) extends TermPlace {

  override def equals( o: Any ) = o match {
    case that: QVar => this.name == that.name
    case _ => false
  }
  override def hashCode = name.hashCode

  def rend = QVar.rendering( name )
  
  override val toString = rend
}

class NumQVar extends QVar( "v" + QVar.next )

object X extends QVar( "X" )
object X1 extends QVar( "X1" )
object X2 extends QVar( "X2" )
object X3 extends QVar( "X3" )

case class TemplateTriple( s: TermPlace, p: TermPlace, o: TermPlace )
extends Tuple3[TermPlace, TermPlace, TermPlace](s, p, o) 
{
  lazy val rend = s.rend + " " + p.rend + " " + o.rend + " ."
}

object TemplateFactory {
  def templateFrom( g: Graph ): Iterable[TemplateTriple] = {
    val tf = new TemplateFactory
    g.triples map{ tf( _ ) }
  }
}

class TemplateFactory {
  val varMap = scala.collection.mutable.Map[ Blank, QVar ]()
  
  def replaced( o: Node ) = o match {
    case n: Blank => varMap.getOrElseUpdate( n, QVar() )
    case other => other
  }
  
  def apply( t: Triple ) = TemplateTriple( replaced( t.sub ), t.pred, replaced( t.obj ) )
}

trait QueryEngine {
  def select( q: String ): List[Map[QVar, Node]]
  def construct( qStr: String ): Graph
}
