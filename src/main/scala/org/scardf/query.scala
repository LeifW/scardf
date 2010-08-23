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
{
  lazy val rend = s.rend + " " + p.rend + " " + o.rend + " ."
}

case class TemplateGraph( v: QVar, ttriples: Iterable[TemplateTriple] ) {
  def rend = ttriples map {_.rend} mkString "\n"
  
  private def queryResult( dg: Graph with QueryEngine ) = {
    val selectQ = "SELECT " + v.rend + " WHERE {" + rend + "}"
    val qresult = dg.select(selectQ)
    println( selectQ, qresult )
    qresult
  }
  
  def findIn( dg: Graph with QueryEngine ): Node = queryResult(dg)(0)(v)
  def findAllIn( dg: Graph with QueryEngine ): List[Node] = queryResult(dg) map { _.apply(v) }
}

object TemplateFactory {
  def apply( assignments: Pair[Blank, QVar]* ) = {
    val tf = new TemplateFactory
    tf.varMap ++ assignments
    tf
  }
}

class TemplateFactory {
  val varMap = scala.collection.mutable.Map[ Blank, QVar ]()
  
  def replaced( o: Node ) = o match {
    case n: Blank => varMap.getOrElseUpdate( n, QVar() )
    case other => other
  }
  
  def apply( t: RdfTriple ): TemplateTriple = TemplateTriple( replaced( t.subj ), t.pred, replaced( t.obj ) )
  
  def apply( g: Graph ): TemplateGraph = varMap.toList match {
    case List( Pair( _, v ) ) => TemplateGraph( v, g.triples map apply )
    case _ => null //TODO ???
  }
}

trait QueryEngine {
  def select( q: String ): List[Map[QVar, Node]]
  def construct( qStr: String ): Graph
}
