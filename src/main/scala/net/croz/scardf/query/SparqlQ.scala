package net.croz.scardf.query

import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.query.{
  Query, QueryExecution, QueryExecutionFactory, QueryFactory, QueryParseException, QuerySolutionMap
}

abstract class SparqlQ[+T <: SparqlQ[T]] extends util.Logging {
  var conditions = new StringBuffer()
  var optConditions = new StringBuffer()
  var orderBySeq = new StringBuffer()
  var upperBound = 0
  var offsetAmount = 0

  //TODO! move these to BaseSelectQ?
  
  def where( triples: (Any, Any, Any)* ): T = appendTriplets( conditions, triples: _* )
  def optional( triples: (Any, Any, Any)* ): T = appendTriplets( optConditions, triples: _* )
  
  def orderBy( comparators: OrderComparator* ): T = {
    for ( c <- comparators ) orderBySeq append c.rendering + " "
    this.asInstanceOf[T]
  }
  
  def limit( n: Int ) = {
    upperBound = n
    this.asInstanceOf[T]
  }

  def offset( n: Int ) = {
    offsetAmount = n    
    this.asInstanceOf[T]
  }
    
  def appendTriplet( sbuffer: StringBuffer, s: Any, p: Any, o: Any ): T = {
    val segments = List( s, p, o ) map rendering
    sbuffer append segments.reduceLeft{ (x: String, y: String) => x + " " + y } + " . "
    this.asInstanceOf[T]
  }

  def appendTriplets( sbuffer: StringBuffer, triples: (Any, Any, Any)* ): T = {
    for ( t <- triples ) appendTriplet( sbuffer, t._1, t._2, t._3 )
    this.asInstanceOf[T]
  }
  
  def rendering( o: Any ): String = o match {
    case qa: QualifiedArguments => qa.modifier + " " + rendering( qa.exprs )
    case seq: Seq[_] => seq map { x: Any => rendering( x ) } mkString " "
    case n: Node => n.rendering
    case s: String => "\"" + s + "\""
    case s: Symbol => QVar.rendering( s.name )
    case x => x.toString
  }
  
  def optionalConditions = if ( optConditions.length == 0 ) "" 
                           else " OPTIONAL { " + optConditions + "}"
  def orderByClause = if ( orderBySeq.length == 0 ) "" else " ORDER BY " + orderBySeq
  def limitClause = if ( upperBound == 0 ) "" else " LIMIT " + upperBound
  def offsetClause = if ( offsetAmount == 0 ) "" else " OFFSET " + offsetAmount

  def execution( rmodel: Model, query: String ) = {
    log.info( "Executing query " + query )
    try {
      val q = QueryFactory.create( query )
      QueryExecutionFactory.create( q, rmodel.jModel, new QuerySolutionMap )
    }
    catch {
      case e: QueryParseException =>
        throw new RuntimeException( "Failed parsing \"" + query + "\" because of" + e.getMessage, e)
    }
  }

  def execution( service: String, query: String ) = {
    log.info( "Executing query " + query )
    try {
      val q = QueryFactory.create( query )
      QueryExecutionFactory.sparqlService( service, q)
    }
    catch {
      case e: QueryParseException =>
        throw new RuntimeException( "Failed parsing \"" + query + "\" because of" + e.getMessage, e)
    }
  }
}

class DescribeQ( v: QVar ) extends SparqlQ[DescribeQ] {
  def from( rmodel: Model ) = descriptionFor( rmodel )

  def descriptionFor( rmodel: Model ) = {
    val query = "DESCRIBE " + v + " WHERE { " + conditions + "}"
    Model( execution( rmodel, query ).execDescribe )
  }
}

class DescribeResQ( r: Res ) extends SparqlQ[DescribeResQ] {
  def in( rmodel: Model ) = Model( execution( rmodel, "DESCRIBE " + r ).execDescribe )
}

abstract class BaseSelectQ[ T <: BaseSelectQ[T] ] extends SparqlQ[T] {

  var selectExprs: List[_] = List()
  
  def queryStr = "SELECT " + rendering( selectExprs ) + 
    " WHERE { " + conditions + optionalConditions + "}" +
    orderByClause + limitClause + offsetClause
  
  def executeOn( model: Model ) = new QResultsIterator( execution( model, queryStr ).execSelect )
  def executeOn( service: String ) = new QResultsIterator( execution( service, queryStr ).execSelect )
  
  def option( solutions: QResultsIterator, v: QVar ): Option[Node] = {
    if ( !solutions.hasNext ) return None
    val result = Some( solutions.next.get( v ).get )
    if ( solutions.hasNext ) throw new RuntimeException( "Multiple solutions to " + this )
    result
  }
}

class SelectQ( exprs: Any* ) extends BaseSelectQ[SelectQ] {
  selectExprs = exprs.toList
  
  def this( selectExpr: Any, conds: StringBuffer ) = {
    this( selectExpr )
    conditions = conds
  }
  
  def from( model: Model ) = executeOn( model )
  def from( service: String ) = executeOn( service )
}

class SelectIteratorQ[T]( converter: NodeConverter[T] ) 
extends BaseSelectQ[SelectIteratorQ[T]] {
  selectExprs = List( X )
  def from( model: Model ): Iterator[T] = executeOn( model ) map { _( X )/converter }
}

class SelectOptionQ[T]( converter: NodeConverter[T] ) extends BaseSelectQ[SelectOptionQ[T]] {
  selectExprs = List( X )
  def from( model: Model ): Option[T] = option( executeOn( model ), X ) map { _/converter }
}

abstract class BaseExtractQ[T <: BaseExtractQ[T]]( val r: Res, replaces: Map[Res, QVar] )
extends BaseSelectQ[T] {
  selectExprs = List( distinct( X ) )
  private var condStr = r.model.dumpedIn( "N-TRIPLE" ).replaceAll( "\\s+", " " )
  condStr = replaceVar( condStr, r, X )
  for ( pair <- replaces )
    condStr = replaceVar( condStr, pair._1, pair._2 )
  conditions = new StringBuffer( condStr )
  
  private def replaceVar( qs: String, r: Res, v: QVar ) =
    qs.replace( NTripleHelper.ntRendering( r.jResource ), rendering( v ) )
}

class ExtractResQ( override val r: Res, replaces: Map[Res, QVar] )
extends BaseExtractQ[ExtractResListQ]( r, replaces ) {
  def from( model: Model ): Option[Res] = option( executeOn( model ), X ) map { _.asRes }
}

class ExtractResListQ( override val r: Res, replaces: Map[Res, QVar] )
extends BaseExtractQ[ExtractResListQ]( r, replaces ) {
  def from( model: Model ): List[Res] = {
    val l = executeOn( model ).toList
    l map { _( X ).asRes }
  }
}

class ConstructQ( triplets: (Any, Any, Any)* ) extends SparqlQ[ConstructQ] {
  var constructions = new StringBuffer()
  appendTriplets( constructions, triplets: _* )
  
  def from( rmodel: Model ) = constructionFrom( rmodel )
  def constructionFrom( rmodel: Model ) = {
    val query = "CONSTRUCT { " + constructions + "} WHERE { " + conditions + "}"
    Model( execution( rmodel, query ).execConstruct )
  }
}

class PTreeConstructQ( ptree: PredicateTree ) extends SparqlQ[PTreeConstructQ] {
  def from( anchor: Res ) = {
    var constructions, required, optionals = new StringBuffer()
    val allTriplets = TripletFactory tripletsFrom ptree.growTemplateFrom( anchor ).model
    appendTriplets( constructions, allTriplets: _* )
    val query = "CONSTRUCT { " + constructions + "} WHERE { " + constructions + "}"
    val markStart = System.currentTimeMillis
    val result = Model( execution( anchor.model, query ).execConstruct )
    log info "Span construction took " + (System.currentTimeMillis - markStart) + " ms"
    result
  }
}

class AskQ( triplets: (Any, Any, Any)* ) extends SparqlQ[AskQ] {
  where( triplets: _* )
  def in( model: Model ) = execution( model, "ASK { " + conditions + "}" ).execAsk
}

class ExtractQ( props: Prop* ) extends SparqlQ[ExtractQ] {
  def from( focus: Res ) = {
    val triplets = new scala.collection.mutable.ListBuffer[(Any, Any, Any)]()
    for ( p <- props ) triplets += (focus, p, QVar())
    new ConstructQ( triplets: _* ) where( triplets: _* ) from focus.model
  }
}

class TakeQ( exprs: Any* ) extends SparqlQ[TakeQ] {
  private var putModel: Model = null
  
  def to( m: Model ) = { putModel = m; this }
  
  private def append( m: Model, subject: Res, predicate: Prop ) = {
    val objects = subject/predicate
    for ( o <- objects ) 
      m add Stmt( subject, predicate, o )
  }
  
  private def appendAll( m: Model, focus: Res, exprs: Collection[Any] ): Unit =
    for ( o <- exprs ) o match {
      case set: Collection[_] => appendAll( m, focus, set.toSeq )
      case p: Prop => append( m, focus, p )
      case pair: Pair[Prop, List[Any]] =>
        val predicate = pair._1
        append( m, focus, predicate )
        for ( r <- focus/predicate ) 
          appendAll( m, r.asRes, pair._2 )
      case _ => throw new RuntimeException( "Unknown TAKE expression " + o )
    }
  
  def from( focus: Res ) = {
    log debug "From " + exprs
    val result = if ( putModel == null ) new Model else putModel
    appendAll( result, focus, exprs.toList )
    result
  }
}

class LocateSpanQ( focus: Res ) {
  def span( ptree: PredicateTree ) = new SelectQ()
}
