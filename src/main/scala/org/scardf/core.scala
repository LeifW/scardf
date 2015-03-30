package org.scardf

import org.joda.time.LocalDate
import scala.language.implicitConversions

/**
 * A node or a variable, something that can be placed as a term in a triple.
 */
trait TermPlace {
  /** A canonical string rendering of this node or variable. */
  def rend: String
}

/**
 * Either a subject node in the context of a graph,
 * or a literal by itself.
 * @see org.scardf.GraphNode
 * @see org.scardf.Literal
 */
trait NodeFromGraph {
  /** The node itself. This object if it is a [[org.scardf.Literal]]. */
  def node: Node
  def /( ur: UriRef ): NodeBag = NodeBag.empty
  def /[T]( nc: NodeToValueConverter[T] ): T = nc( this )
}

/**
 * RDF node; either a URI reference, a blank node, or a literal.
 */
sealed abstract class Node() extends TermPlace {
  /**
   * If this is a non-literal, constructs a   
   */  
  def apply( g: Graph ): NodeFromGraph
  
  /** Is this node a [[org.scardf.Blank]]]? */
  def isBlank = isInstanceOf[Blank]
  
  /**
   * Is this node a [[org.scardf.Literal]]?
   * @see Literal
   */        
  def isLiteral = isInstanceOf[Literal]
}

object Node {
  /**
   * Constructs a node from given value.
   * <li>for a Node, returns it</li>
   * <li>for a GraphNode, returns its node</li>
   * <li>for a String, returns a [[org.scardf.PlainLiteral]]</li>
   * <li>for given object of class Int, Long, BigInt, BigDecimal, Boolean, LocalDate:
   *     constructs an appropriate typed literal and returns it</li>
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
   * <li>every node matches object Node: `matches` always returns `true`</li>
   * <li>only literal nodes match object [[org.scardf.Literal]]</li>
   * <li>only plain literals match object PlainLiteral</li>
   * <li>only typed literals match object TypedLiteral</li>
   * <li>only subject nodes match object SubjectNode</li>
   * <li>only blank node match object Blank</li>
   * <li>only URI reference match object UriRef</li>
   * <li>if template is a node, parameters are checked for equality</li>
   * <li>if template is a graph node, its node is checked for equality</li>
   * <li>if template is function of Node to Boolean, this function is applied to tested node;
   *     any exception thrown from this function is absorbed and `false` is returned</li>
   * <li>if template is [[scala.None]], `false` is returned</li>
   * <li>if template is [[scala.Some]], this method is reapplied with the content being
   *     the template parameter</li>
   * <li>in all other cases, a node is constructed from the template object using
   *     the method `from`, and this node is compared to the tested node `n`</li>
   */
  def matches( template: Any, n: Node ): Boolean = template match {
    case Node => true
    case Literal => n.isLiteral
    case PlainLiteral => n.isInstanceOf[PlainLiteral]
    case TypedLiteral => n.isInstanceOf[TypedLiteral]
    case SubjectNode => n.isInstanceOf[SubjectNode]
    case UriRef => n.isInstanceOf[UriRef]
    case Blank => n.isBlank
    case _: Node => template == n
    case gn: GraphNode => gn.node == n
    case fn: Function[Node @unchecked,Boolean @unchecked] => try { fn( n ) } catch { case e: Exception => false }
    case None => false
    case Some(x) => matches( x, n )
    case _ => (Node from template) == n
  }

  override def toString = "Node"
}

/**
 * RDF node that can be used as a subject of a triple.
 * Either a URI reference, or a blank node.
 */
sealed abstract class SubjectNode() extends Node {
  def -( pred: UriRef ) = SubPredPair( this, pred )
  
  def -( poPairs: (UriRef, Any)* ) = Branch.make( this, poPairs: _* )
  
  override def apply( g: Graph ) = g/this
}
object SubjectNode

/**
 * URI reference, a kind of a subject node.
 * Two URI references are equal if their URI strings are equal (char-per-char).
 */
class UriRef( val uri: String ) extends SubjectNode with NodeToBagConverter {
  override def apply( nfg: NodeFromGraph ) = nfg match {
    case gn: GraphNode => gn/this
    case _ => throw new RdfTraversalException( "Cannot traverse a predicate from " + nfg )
  }
  
  def ~( subs: PredicateBranch* ) = PredicateBranch( this, true, true, PredicateTree( subs: _* ) )
  /** Equivalent to (-this)~(...) */ 
  def <~( subs: PredicateBranch* ) = PredicateBranch( this, false, true, PredicateTree( subs: _* ) )
  /** Equivalent to (-this?)~(...) */
  def ?<~( subs: PredicateBranch* ) = PredicateBranch( this, false, false, PredicateTree( subs: _* ) )
  /** Equivalent to (this?)~(...) */
  def ?~( subs: PredicateBranch* ) = PredicateBranch( this, true, false, PredicateTree( subs: _* ) )
  def ? = PredicateBranch( this, true, false, PredicateTree.empty )
  def unary_- = PredicateBranch( this, false, true, PredicateTree.empty )

  override def rend = "<" + uri + ">"

  final def canEqual(other: Any): Boolean = other.isInstanceOf[UriRef]

  final override def equals(other: Any): Boolean =
    other match {
      case that: UriRef => (that canEqual this) && this.uri == that.uri
      case _ => false
    }
 
  final override def hashCode = uri.hashCode
  
  override val toString = rend
}

object UriRef {
  def apply( uri: String ) = new UriRef( uri )
  def unapply( ur: UriRef ): Option[String] = Some( ur.uri )
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
sealed abstract class Literal( val lexicalForm: String ) extends Node with NodeFromGraph {
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
  implicit def toTypedLiteral( i: Int ) = TypedLiteral( i.toString, XSD.int )
  implicit def toTypedLiteral( i: BigInt ) = TypedLiteral( i.toString, XSD.integer )
  implicit def toTypedLiteral( i: Long ) = TypedLiteral( i.toString, XSD.long )
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
  def langExt = langTagOpt map {"@" + _.code} getOrElse ""
  override def rend = "\"" + escapedLexicalForm + "\"" + langExt
  override def toString = "PlainLiteral(\"" + lexicalForm + "\"" + langExt + ")"
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
  override def toString = "TypedLiteral(\"" + lexicalForm + "\", " + datatypeUriRef.rend + ")"
}

/**
 * A language tag, used in @{link PlainLiteral plain literals}.
 * Identity of the tag is its [[#code]], forced to be lowercase. 
 */ 
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
