package net.croz.scardf

import java.io.StringWriter
import com.hp.hpl.jena.rdf.model.AnonId
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.rdf.model.RDFList
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.Statement
import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.{Model => JModel}
import com.hp.hpl.jena.ontology.OntModel

class Model( val jModel: JModel ) extends util.Logging {

  val internalId = Model.rnd.nextLong
  Model remember this

  var prefix = ""
  
  val mapping = scala.collection.mutable.Map[RDFNode, Node]()
  val stmtMapping = scala.collection.mutable.Map[Statement, Stmt]()

  def this() = this( ModelFactory.createOntologyModel )
  
  def regNs( namespaces: Map[String, String] ) = 
    namespaces foreach { e => jModel.setNsPrefix( e._1, e._2 ) }
  def regNs( pvMappings: Pair[String, Vocabulary]* ): Unit = 
    regNs( Map( pvMappings map { p => (p._1, p._2.prefix) }: _* ) )
    
  def withPrefix( prefix: String ) = { this.prefix = prefix; this }
  
  private def remember( r: Res ) = {
    //log.info( hashCode + " " + r + " " + mapping )
    mapping += r.jResource -> r
    r
  }
  
  def getAnon() = remember( new Res( jModel.createResource( new AnonId() ), this ) )
  def getAnon( id: String ) = remember( new Res( jModel createResource new AnonId( id ), this ) )
  
  def getRes( jRes: Resource ): Res = {
    //log.info( this + " " + jRes + " " + (mapping get jRes) + " " + mapping )
    mapping.getOrElseUpdate( jRes, newRes( jRes ) ).asInstanceOf[Res]
  }
  
  def getRes( uri: String ): Res = getRes( jModel.getResource( this.prefix + uri ) )
  
  def /( res: Res ) = getRes( res.jResource )
  
  private def newRes( jResource: Resource ) = {
    if ( jResource.canAs( classOf[RDFList] ) )
      new RdfList( jResource.as( classOf[RDFList] ).asInstanceOf[RDFList], this )
    else if ( jResource.canAs( classOf[Property] ) )
      new Prop( jResource.as( classOf[Property] ).asInstanceOf[Property], this )
    else if ( jResource.isURIResource )
      new Res( jModel createResource jResource.getURI, this )
    else
      getAnon( jResource.getId.getLabelString )
  }
  
  def getProp( jProp: Property ): Prop = 
    mapping.getOrElseUpdate( jProp, new Prop( jProp, this ) ).asInstanceOf[Prop]
  def getProp( uri: String ): Prop = getProp( jModel.getProperty( this.prefix + uri ) )

  def getRdfList( jRdfList: RDFList ): RdfList = {
    //log.info( this, "get list", jRdfList, mapping.get(jRdfList), mapping )
    val r = mapping.getOrElseUpdate( jRdfList, new RdfList( jRdfList, this ) ).asInstanceOf[RdfList]
    //log.info( this, jRdfList, mapping.get(jRdfList), mapping )
    r
  }
  
  def getStatement( jStatement: Statement ): Stmt =
    stmtMapping.getOrElseUpdate( jStatement, new Stmt( jStatement, this ) )

  def statements = new RichStmtIterator( jModel.listStatements )

  def add( stmt: Stmt ) = jModel add stmt.jStatement
  
  def addAll( stmts: List[Stmt] ) = stmts map add
  
  def ++( other: Model ) = Model( jModel add other.jModel )
  
  def listRes( assignment: Pair[Prop, Any] ) = { 
    val jp = assignment._1.jProperty
    val result = assignment._2 match {
      case n: Node => jModel.listResourcesWithProperty( jp, n.jNode )
      case o => jModel.listResourcesWithProperty( jp, o )
    }
    new RichResIterator( result ) 
  }

  def local = Model( jModel match {
    case om: OntModel => om.getBaseModel
    case m => m
  } )

  def dump = jModel.write( System.out, "TURTLE" )
  
  def dumpAll = jModel match {
    case om: OntModel => om.writeAll( System.out, "TURTLE", null )
    case _ => dump
  }
  
  def dumpStatements = for ( s <- statements ) println( s )
  
  def dumpedIn( syntax: String ) = {
    val sw = new StringWriter
    jModel.write( sw, syntax )
    sw.toString
  }
  
  def dumped = dumpedIn( "TURTLE" )

  override def equals( that: Any ) = that match {
    case m: Model => jModel == m.jModel
    case _ => false
  }
  override def hashCode = jModel.hashCode
  
  def =~( that: Model ) = jModel isIsomorphicWith that.jModel
  
  override def toString = java.lang.Long.toHexString( internalId ) + "/" + jModel.hashCode
}

object Model {
  val mapping = scala.collection.mutable.Map[JModel, Model]()
  val rnd = new java.util.Random()
  
  private def remember( m: Model ) = {
    mapping += m.jModel -> m
    m
  }

  def apply(): Model = apply( null )
  def apply( jm: JModel ): Model = {
    val rm = if ( jm == null ) new Model 
             else mapping.getOrElse( jm, new Model( jm ) )
    rm
  }
  def newDefault = new Model( ModelFactory.createDefaultModel )
  
  def construct( expr: => Any ) = {
    implicit val model = new Model
    expr
    model
  }
  
  implicit def toRModel( jm: JModel ) = Model( jm )
  implicit def toJModel( rm: Model ) = rm.jModel
}
