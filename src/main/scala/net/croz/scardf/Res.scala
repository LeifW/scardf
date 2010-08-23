package net.croz.scardf

import org.joda.time.LocalDate
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import com.hp.hpl.jena.rdf.model.{ RDFNode, Resource, ResIterator }
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype._
import scala.collection.mutable.{Set => MSet}

class Res( val jResource: Resource, val model: Model ) 
extends Node( jResource ) with util.Logging {

  val uri = jResource.getURI

  def apply( a: (Prop, Any) ) = Stmt( this, a._1, a._2 )
  def apply( assignments: (Prop, Any)* ) =
    for ( a <- assignments ) yield Stmt( this, a._1, a._2 )
  
  def apply( m: Model ) = this in m

  def has( assignment: (Prop, Any) ) = assignment match {
    case (p, None) => !valuesOf( p ).hasNext
    case _ => this( assignment )?
  }
  
  override def /( p: Prop ): NodeBag = new NodeBag( valuesOf( p ).toList )
  override def /( pp: PropPath ): NodeBag = pp.foldLeft( NodeBag( this ) ){ _/_ }

  def valueOf( p: Prop ): Option[Node] = {
    if ( jResource hasProperty p.jProperty ) 
      Some( Node( jResource getProperty p.jProperty getObject ) ) 
    else
      None
  }

  def valuesOf( p: Prop ): Iterator[Node] = {
    log.debug( this + " valuesof " + p + " = " + (jResource listProperties p.jProperty).toList )
    new RichStmtIterator( jResource listProperties p.jProperty ) map {_.o}
  }

  def in( m: Model ) = m getRes jResource
  def local = this in model.local
  
  def a( rdfClass: Res ) = state( RDF.Type -> rdfClass )
  def an( rdfClass: Res ) = a( rdfClass )

  def state( assignments: (Prop, Any)* ) = { 
    assignments foreach { a => assign( a._1, a._2 ) }
    this
  }
  
  /**
   * <ul>
   *   <li>Scardf or Jena node - assign this node</li>
   *   <li>All object - assign each value in All</li>
   *   <li>an Option - assign some value if defined, skip if None</li>
   *   <li>a tuple - assign each tuple member</li>
   *   <li>a string - assign string as prop is typed, or xsd:string if no type is specified</li>
   *   <li>a LangStr - assign string value in a language</li>
   *   <li>a Boolean</li>
   *   <li>an Int</li>
   *   <li>a LocalDate</li>
   *   <li>a DateTime</li>
   *   <li>otherwise, throw a RuntimeException</li>
   * </ul>
   */
  def assign( prop: Prop, value: Any ): Res = {
    value match {
      case n: Node      => jResource.addProperty( prop, n.jNode )
      case jn: RDFNode  => jResource.addProperty( prop, jn )
      case all: All     => for ( n <- all.nodes ) assign( prop, n )
      case Some( x )    => assign( prop, x )
      case None         => // ignore assignment
      case (a, b)       => assign( prop, a ); assign( prop, b )
      case (a, b, c)    => assign( prop, a ); assign( prop, b ); assign( prop, c )
      case (a, b, c, d) => assign( prop, All( a, b, c, d ) )
      case s: String    => jResource.addProperty( prop, s, prop.datatype.getOrElse( XSDstring ) )
      case b: Boolean   => jResource.addProperty( prop, b.toString, XSDboolean )
      case i: Int       => jResource.addProperty( prop, i.toString, XSDint )
      case d: LocalDate => jResource.addProperty( prop, d.toString, XSDdate )
      case d: DateTime  => jResource.addProperty( prop, ISODateTimeFormat.dateTime.print( d ), XSDdateTime )
      case x            => throw new RuntimeException( x + " of unknown type" )
    }
    this
  }
  
  def -( p: Prop ) = ResPropPair( this, p )

  def isOfType( checkType: Res ) = this/RDF.Type contains checkType

  def subgraphed = {
    val g = new Model
    val covered = MSet[Res]()
    spreadTo( g, covered )
    g getRes this.jResource
  }
  
  def spreadTo( subgraph: Model, covered: MSet[Res] ): Unit = {
    if ( covered contains this ) {
      log debug "spreading already covered " + this 
      return
    }
    covered += this
    log debug "spreading subgraph to " + this
    val outlinks = new RichStmtIterator(
      model.local.jModel.listStatements( jResource, null, null: RDFNode )
    )
    val connectedNodes = scala.collection.mutable.Set[Node]()
    for ( s <- outlinks ) {
      connectedNodes += s.p
      connectedNodes += s.o
      subgraph add s
    }
    connectedNodes filter { _.isRes } map { _.asRes.spreadTo( subgraph, covered ) }
  }
  
  override def rendering: String =
    if ( jResource.isAnon ) "_:A" + jResource.getId.getLabelString.replace( ":", "" )
    else "<" + uri + ">"
}

object Res {
  def apply( uri: String )( implicit rmodel: Model ) = rmodel getRes uri

  def apply()( implicit rmodel: Model ) = rmodel.getAnon
  
  def apply( r: Resource ): Res = apply( r, Model( r.getModel ) )
  def apply( r: Resource, m: Model ) = m getRes r
  
  implicit def toRes( r: Resource ) = apply( r )
  implicit def toNodeBag( rri: RichResIterator ) = new NodeBag( rri.toList )
}

object Anon {
  def apply( assignments: (Prop, Any)* )( implicit rmodel: Model ) =
    Res().state( assignments: _* )
  
  def apply( rdfClass: Res, assignments: (Prop, Any)* )( implicit rmodel: Model ) = 
    Res().a( rdfClass ).state( assignments: _* )
  
  def apply( id: String )( implicit rmodel: Model ) = rmodel.getAnon( id )
}

object Blank {
  val model = new Model
  
  def apply( assignments: (Prop, Any)* ): Subgraph = {
    val subg = new Subgraph
    assignments.toList map { p => p._2 match {
      case n: Node => subg += (p._1, n)
      case sg: Subgraph => subg += (p._1, sg)
      case x: Any => subg += (p._1, Lit from x)
    } }
    subg
  }
  
  implicit def toRModel( sg: Subgraph ) = sg.toModel
}

case class All( nodes: Any* )

class Subgraph {
  val root = Blank.model.getAnon
  var slist = new scala.collection.mutable.ListBuffer[Stmt]()
  def +=( p: Prop, n: Node ) = slist += Stmt( root, p, n )
  def +=( p: Prop, sg: Subgraph ) = slist ++= ( Stmt( root, p, sg.root ) :: sg.slist.toList )
  def toModel = {
    val rm = new Model
    rm addAll slist.toList
    rm
  }
}

class RichResIterator( jIterator: ResIterator ) extends Iterator[Res] {
  override def hasNext = jIterator.hasNext
  override def next = Res( jIterator.next.asInstanceOf[Resource] )
}

case class ResPropPair( s: Res, p: Prop ) {
  def ->( o: Any ) = s.state( p -> o)
}
