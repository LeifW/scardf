package org.scardf.jena

import org.scardf._
import scala.collection.mutable.ArrayBuffer
import com.hp.hpl.jena.rdf.model.{Literal => JLiteral, Property => JProperty, _}
import com.hp.hpl.jena.datatypes.TypeMapper
import com.hp.hpl.jena.query._

class JenaGraph( private[jena] val m: Model ) extends MutableGraph with QueryEngine {
  
  def this() = this( ModelFactory.createDefaultModel )

  def triples = new JenaTripleIterator( m.listStatements ).toList

  override def triplesLike( sp: Any, pp: Any, op: Any ): Iterable[RdfTriple] = {
    import Node.matches
    val tt = new JenaTripleIterator( m.listStatements( 
      sp match {
        case n: SubjectNode => resource( n )
        case _ => null
      }, 
      pp match {
        case n: UriRef => property( n )
        case _ => null
      }, 
      op match {
        case n: Node => rdfnode( n )
        case _ => null
      } 
    ) )
    tt.filter{
      case RdfTriple( s, p, o ) => matches( sp, s ) && matches( pp, p ) && matches( op, o ) 
      case _ => false
    }.toList
  }

  def contains( t: RdfTriple ) = m contains statement( t )

  override def +( t: RdfTriple ) = new JenaGraph ++= triples += t
  
  override def +=( t: RdfTriple ) = {
    m add statement( t )
    this
  }

  override def =~( that: Graph ): Boolean = that match {
    case jg: JenaGraph => m isIsomorphicWith jg.m
    case g => super.=~( g )
  }
  
  override def subjects: Set[SubjectNode] = Set() ++ new JenaResIterator( m.listSubjects )

  def statement( t: RdfTriple ): Statement = 
    m.createStatement( resource( t.subj ), property( t.pred ), rdfnode( t.obj ) )
  
  def resource( sn: SubjectNode ) = sn match {
    case b: Blank => m createResource new AnonId( b.id )
    case u: UriRef => property( u )
  }
  
  def property( p: UriRef ) = m.createProperty( p.uri )
  
  def rdfnode( n: Node ): RDFNode = n match {
    case sn: SubjectNode => resource( sn )
    case PlainLiteral( lf, None ) => ResourceFactory.createPlainLiteral( lf )
    case PlainLiteral( lf, Some( langtag ) ) => m.createLiteral( lf, langtag.code )
    case TypedLiteral( lf, UriRef( dtUri ) ) => 
      ResourceFactory.createTypedLiteral( lf, TypeMapper.getInstance.getSafeTypeByName( dtUri ) )
  }

  override def ++=( ts: TraversableOnce[RdfTriple] ): JenaGraph = { ts foreach +=; this }

  override def ++( ts: TraversableOnce[RdfTriple] ): JenaGraph = new JenaGraph() ++= triples ++= ts

  def select( qStr: String ): List[Map[QVar, Node]] = {
    val q = QueryFactory.create( qStr )
    val e = QueryExecutionFactory.create( q, m, new QuerySolutionMap )
    val rs = new QResultsIterator( e.execSelect, m )
    rs.solutions
  }
  
  def construct( qStr: String ): Graph = {
    val q = QueryFactory.create( qStr )
    val e = QueryExecutionFactory.create( q, m, new QuerySolutionMap )
    new JenaGraph( e.execConstruct )
  }
}

class JenaSerializator( val sf: SerializationFormat) extends Serializator( sf ) {
  override def write( g: Graph, w: java.io.Writer ) = {
    val m = g match {
      case jg: JenaGraph => jg.m
      case _ => (new JenaGraph ++ g).asInstanceOf[JenaGraph].m
    }
    bindings foreach { p => m.setNsPrefix( p._1, p._2 ) }
    m.write( w, Jena codeFor sf )
  }
  
  override def readFrom( r: java.io.Reader ) = {
    val jg = new JenaGraph
    jg.m.read( r, null, Jena codeFor sf )
    jg
  }
}

class JenaResIterator( jIterator: ResIterator ) extends Iterator[SubjectNode] {
  override def hasNext = jIterator.hasNext
  override def next = Jena.subjectNode( jIterator.next.asInstanceOf[Resource] )
}

class JenaTripleIterator( jIterator: StmtIterator ) extends Iterator[RdfTriple] {
  override def hasNext = jIterator.hasNext
  override def next = Jena.triple( jIterator.next.asInstanceOf[Statement] )
}

object Jena {
  def subjectNode( r: Resource ): SubjectNode = 
    if ( r.isAnon ) Blank( r.getId.getLabelString ) else uriRef( r )
  
  def uriRef( r: Resource ) = UriRef( r.getURI )
  
  def node( n: RDFNode ): Node = n match {
    case null         => null
    case p: JProperty => uriRef( p )
    case r: Resource  => subjectNode( r )
    case l: JLiteral  => literal( l )
  }
  
  def literal( l: JLiteral ) = {
    val lexForm = l.getLexicalForm
    val typeUri = l.getDatatypeURI
    if ( typeUri == null ) PlainLiteral( lexForm, LangTag.opt( l.getLanguage ) )
    else TypedLiteral( lexForm, UriRef( typeUri ) )
  }
  
  def triple( s: Statement ) = RdfTriple( 
    subjectNode( s.getSubject ), 
    uriRef( s.getPredicate ), 
    node( s.getObject ) 
  )
  
  def codeFor( sf: SerializationFormat ) = sf match {
    case RdfXml => "RDF/XML"
    case NTriple => "N-TRIPLE"
    case Turtle => "TURTLE"
    case N3 => "N3"
    case _ => null
  }
}
