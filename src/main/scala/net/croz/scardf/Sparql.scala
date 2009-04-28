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
  def selectX[T]( c: NodeConverter[T] ) = new SelectOptionQ( c )
  def selectAllX[T]( c: NodeConverter[T] ) = new SelectIteratorQ( c )
  
  def extractRes( r: Res, replaces: Pair[Res, QVar]* ) = new ExtractResQ( r, Map( replaces: _* ) )
  def extractResList( r: Res, replaces: Pair[Res, QVar]* ) = 
    new ExtractResListQ( r, Map( replaces: _* ) )
  def extract( props: Prop* ) = new ExtractQ( props: _* )
  
  def ask( triplets: (Any, Any, Any)* ) = new AskQ( triplets: _* )
  
  def describe( v: QVar ) = new DescribeQ( v )
  def descriptionOf( r: Res ) = new DescribeResQ( r ) in r.model
  
  def construct( triplets: (Any, Any, Any)* ) = new ConstructQ( triplets: _* )
  def construct( tempGraph: Model ): ConstructQ = {
    val triplets = TripletFactory tripletsFrom tempGraph
    construct( triplets: _* ) where( triplets: _* )
  }
  def construct( ptree: PredicateTree ) = new PTreeConstructQ( ptree )
  
  def take( expr: Any* ) = new TakeQ( expr: _* )
}

sealed abstract class QualifiedArguments( val modifier: String, val exprs: Any* )
case class distinct( override val exprs: Any* ) extends QualifiedArguments( "DISTINCT", exprs )
case class reduced( override val exprs: Any* ) extends QualifiedArguments( "REDUCED", exprs )

case class QSolution( jSolution: QuerySolution ) {
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
    if ( solution == null ) None else Some( Node( solution ) )
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
 */
case class QResultsIterator( rs: ResultSet ) extends Iterator[QSolution] {
  def hasNext = rs.hasNext
  def next = QSolution( rs.nextSolution )
  
  /**
   * Constructs a list of maps of all solutions from this iterator.
   * @see QSolution#toMap
   */
  def solutions = toList map { _.toMap }
}

abstract class OrderComparator( v: QVar, modifier: String ) {
  def rendering = modifier + "( " + v + " )"
}
case class asc( v: QVar ) extends OrderComparator( v, "ASC" )
case class desc( v: QVar ) extends OrderComparator( v, "DESC" )
