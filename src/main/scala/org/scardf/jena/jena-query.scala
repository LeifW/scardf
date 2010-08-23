package org.scardf.jena

import org.scardf._
import scala.collection.mutable.ArrayBuffer
import com.hp.hpl.jena.rdf.model.{Literal => JLiteral, _}
import com.hp.hpl.jena.query._

case class QSolution( jSolution: QuerySolution, m: Model ) {
  /**
   * Value of given variable for this solution.
   * @throws NoSuchElementException if there's no associated value
   */
  def apply( v: QVar ): Node = get( v ).get
  
  def get( v: QVar ): Option[Node] = get( v.name )
  
  /**
   * Optional value of variable given by its name.
   */
  def get( key: String ) = {
    val solution = jSolution.get( key )
    if ( solution == null ) None else Some( converted( solution ) )
  }
  
  def converted( jenaX: Any ) = jenaX match {
    case jnode: RDFNode => Jena.node( jnode )
    case x => Node from x
  }

  /**
   * Constructs a map of all variables and their values for this solution.
   */
  def toMap: Map[QVar, Node] = {
    val result = scala.collection.mutable.Map[QVar, Node]()
    val iterator = jSolution.varNames
    while ( iterator.hasNext ) {
      val qvar = QVar( iterator.next.asInstanceOf[String] )
      get( qvar ) match {
        case Some( value ) => result += qvar -> value
        case None => // skip
      }
    }
    Map.empty ++ result
  }
}

/**
 * 
 */
class QResultsIterator( rs: ResultSet, m: Model ) extends Iterator[QSolution] {
  def hasNext = rs.hasNext
  def next = QSolution( rs.nextSolution, m )
  
  /**
   * Constructs a list of maps of all solutions from this iterator.
   * @see QSolution#toMap
   */
  def solutions = toList map { _.toMap }
}
