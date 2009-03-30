package net.croz.scardf

import net.croz.scardf.query._
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.query.Query
import com.hp.hpl.jena.query.QueryExecution
import com.hp.hpl.jena.query.QueryExecutionFactory
import com.hp.hpl.jena.query.QueryFactory
import com.hp.hpl.jena.query.QueryParseException
import com.hp.hpl.jena.query.QuerySolution
import com.hp.hpl.jena.query.QuerySolutionMap
import com.hp.hpl.jena.query.ResultSet
 
object Sparql {
  def select( exprs: Any* ) = new SelectQ( exprs )
  def selectRes( v: QVar ) = new SelectResQ( v )
  def selectAll = new SelectResIteratorQ( ResX )
  def extractRes( r: Res ) = new ExtractResQ( r )
  def extractResList( r: Res ) = new ExtractResListQ( r )
  def describe( v: QVar ) = new DescribeQ( v )
  def descriptionOf( r: Res ) = new DescribeResQ( r ) in r.model
  def construct( triplets: (Any, Any, Any)* ) = new ConstructQ( triplets: _* )
  def extract( props: Prop* ) = new ExtractQ( props: _* )
  def take( expr: Any* ) = new TakeQ( expr: _* )
}

sealed abstract class QualifiedArguments( val modifier: String, val exprs: Any* )
case class distinct( override val exprs: Any* ) extends QualifiedArguments( "DISTINCT", exprs )
case class reduced( override val exprs: Any* ) extends QualifiedArguments( "REDUCED", exprs )

case class QSolution( jSolution: QuerySolution ) {
  def apply( v: QVar ): Node = get( v ).get
  
  def get( v: QVar ): Option[Node] = get( v.name )
  
  def get( key: String ) = {
    val solution = jSolution.get( key )
    if ( solution == null ) None else Some( new Node( solution ) )
  }

  /**
   * Converts t
   */
  def toMap: Map[QVar, Node] = {
    val result = scala.collection.mutable.Map[QVar, Node]()
    val iterator = jSolution.varNames
    while ( iterator.hasNext ) {
      val v = QVar( iterator.next.asInstanceOf[String] )
      result += v -> apply( v )
    }
    Map.empty ++ result
  }
}

/**
 */
case class QResultsIterator( rs: ResultSet ) extends Iterator[QSolution] {
  def hasNext = rs.hasNext
  def next = QSolution( rs.nextSolution )
  
  /**
   * 
   */
  def solutions = toList map { _.toMap }
}

abstract class OrderComparator( v: QVar, modifier: String ) {
  def rendering = modifier + "( " + v + " )"
}
case class asc( v: QVar ) extends OrderComparator( v, "ASC" )
case class desc( v: QVar ) extends OrderComparator( v, "DESC" )
