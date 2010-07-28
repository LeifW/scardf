package org.scardf

import org.joda.time.LocalDate

/**
 * A node or a variable, something that can be placed as a term in a triple.
 */
trait TermPlace {
  def rend: String
}

/**
 * Either a subject node in the context of a graph,
 * or a literal.
 */
trait NodeFromGraph {
  def node: Node
  def /( ur: UriRef ): NodeBag = NodeBag.empty
  def /[T]( nc: NodeToValueConverter[T] ): T = nc( this )
}

/**
 * RDF node, either a URI reference, a blank node, or a literal.
 */
sealed abstract case class Node() extends TermPlace {
  def apply( g: Graph ): NodeFromGraph
  def isBlank = isInstanceOf[Blank]
  def isLiteral = isInstanceOf[Literal]
}

object Node {
  /**
   * Constructs a node from given value.
   * <li>for a Node, returns it</li>
   * <li>for a GraphNode, returns its node</li>
   * <li>for a String, Int, Long, BigInt, BigDecimal, Boolean, LocalDate, returns it as a typed literal</li>
   * <li>for other values, throws an IllegalArgumentException</li>
   */
  def from( a: Any ): Node = {
    import Literal._
    a match {
      case n: Node => n
      case gn: GraphNode => gn.node
      case s: String => toPlainLiteral( s )
      case i: Int => toTypedLiteral( i )
      case i: Long => toTypedLiteral( i )
      case i: BigInt => toTypedLiteral( i )
      case d: BigDecimal => toTypedLiteral( d )
      case b: Boolean => toTypedLiteral( b )
      case ld: LocalDate => toTypedLiteral( ld )
      case x: AnyRef => throw new IllegalArgumentException( "Cannot convert " + x + " of " + x.getClass + " to Node" )
      case x => throw new IllegalArgumentException( "Cannot convert " + x + " to Node" )
    }
  }

  /**
   * Checks if given node matches given template (any object).
   * <li>every node matches object Node</li>
   * <li>every literal node matches object Literal</li>
   * <li>every subject node matches object SubjectNode</li>
   * <li>every blank node matches object Blank</li>
   * <li>if template is a node, parameters are checked for equality</li>
   * <li>if template is a graph node, its node is checked for equality</li>
   * <li>if template is function of Node to Boolean, this function is applied to tested node</li>
   * <li>in all other cases, a node is constructed from the template object, and this is compared to the tested node</li>
   */
  def matches( template: Any, n: Node ): Boolean = template match {
    case Node => true
    case Literal => n.isLiteral
    case SubjectNode => !n.isLiteral
    case UriRef => !n.isBlank && !n.isLiteral
    case Blank => n.isBlank
    case m: Node => m == n
    case gn: GraphNode => gn.node == n
    case fn: Function[Node, Boolean] => fn( n )
    case v => (Node from v) == n
  }
}

/**
 * RDF node that can be used as a subject of a triple.
 * Either a URI reference, or a blank node.
 */
sealed abstract case class SubjectNode() extends Node {
  def -( pred: UriRef ) = SubPredPair( this, pred )
  
  def -( poPairs: Pair[ UriRef, Any ]* ) = Branch.make( this, poPairs: _* )
  
  override def apply( g: Graph ) = g/this
}

/**
 * URI reference, a kind of a subject node.
 */
case class UriRef( uri: String ) extends SubjectNode with NodeToBagConverter {
  override def apply( nfg: NodeFromGraph ) = nfg match {
    case gn: GraphNode => gn/this
    case _ => throw new RdfTraversalException( "Cannot traverse a predicate from " + nfg )
  }
  
  def ~( subtrees: PredicateTree* ) = PredicateTree( this -> subtrees.reduceLeft( _ ++ _ ) )()
  
  def ?~( subtrees: PredicateTree* ) = PredicateTree.opt( this -> subtrees.reduceLeft( _ ++ _ ) )
  
  def ? = PredicateTree.opt( this -> PredicateTree.empty )
  
  override def rend = "<" + uri + ">"

  def canEqual(other: Any): Boolean = other.isInstanceOf[UriRef]

  override def equals(other: Any): Boolean =
    other match {
      case that: UriRef => (that canEqual this) && this.uri == that.uri
      case _ => false
    }
 
  override def hashCode = uri.hashCode
  
  override val toString = rend
}

/**
 * A blank node.
 */
case class Blank( id: String ) extends SubjectNode {
  override val rend = "_:" + id
  
  override val toString = rend
}

object Blank {
  def apply() = new Blank(
    "b" + java.lang.Long.toHexString( java.util.UUID.randomUUID.getLeastSignificantBits ) 
  )
}

/**
 * A literal.
 */
sealed abstract case class Literal( val lexicalForm: String ) extends Node with NodeFromGraph {
  override final def node = this
  
  def apply( g: Graph ) = this
  
  //TODO needs optimizing
  lazy val escapedLexicalForm: String = {
    val sb = new StringBuffer
    lexicalForm map {
      ch: Char => Literal.escapeMap.getOrElse( ch, ch.toString ) 
    } foreach {
      sb append _
    }
    sb.toString
  }
}

object Literal {
  val escapeMap = Map[Char, String](
    '\t' -> "\\t", '\n' -> "\\n", '\r' -> "\\r", '\"' -> "\\\"", '\\' -> "\\\\" //TODO more...
  )
  
  def apply( o: Any ): Literal = Node from o match {
    case l: Literal => l
    case _ => throw new RuntimeException( "Cannot convert " + o + " to Literal" )
  }
  
  implicit def toPlainLiteral( s: String ) = PlainLiteral( s, None )
  implicit def toTypedLiteral( b: Boolean ) = TypedLiteral( b.toString, XSD.boolean )
  implicit def toTypedLiteral( i: Int ) = TypedLiteral( i.toString, XSD.integer )
  implicit def toTypedLiteral( i: Long ) = TypedLiteral( i.toString, XSD.long )
  implicit def toTypedLiteral( i: BigInt ) = TypedLiteral( i.toString, XSD.long ) //TODO type?
  implicit def toTypedLiteral( d: BigDecimal ) = TypedLiteral( d.toString, XSD.decimal )
  implicit def toTypedLiteral( d: LocalDate ) = TypedLiteral( 
    org.joda.time.format.ISODateTimeFormat.date.print( d ), XSD.date
  )
}

/**
 * Plain literal, a lexic form and optional language tag. 
 */
case class PlainLiteral( override val lexicalForm: String, langTagOpt: Option[LangTag] ) 
extends Literal( lexicalForm ) {
  def @@( lang: LangTag ) = PlainLiteral( lexicalForm, Some( lang ) )
  override def rend = "\"" + escapedLexicalForm + "\"" + ( langTagOpt map { "@" + _.code } getOrElse "" ) 
}

object PlainLiteral {
  def apply( lf: String ) = new PlainLiteral( lf, None )
}

/**
 * Typed literal, consisting of a lexical form and a datatype URI reference.
 */
case class TypedLiteral( override val lexicalForm: String, datatypeUriRef: UriRef )
extends Literal( lexicalForm ) {
  override def rend = "\"" + escapedLexicalForm + "\"^^" + datatypeUriRef.rend
}

sealed class LangTag( c: String ) {
  val code = c.toLowerCase
  
  override lazy val hashCode = code.hashCode
  
  override def equals( a: Any ): Boolean = a match {
    case that: LangTag => this.code == that.code
    case _ => false
  }

  def apply( lf: String ) = PlainLiteral( lf, Some( this ) )
  
  override lazy val toString = code
}

object LangTag {
  def apply( c: String ) = new LangTag( c )
  def opt( c: String ) = if ( c == null || c == "" ) None else Some( apply( c ) )
}
