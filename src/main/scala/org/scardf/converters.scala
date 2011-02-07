package org.scardf

import org.joda.time.{LocalDate, DateTime}
import org.joda.time.format.ISODateTimeFormat.{date => IsoFormat}

/**
 * Node-to-value converters take a single {NodeFromGraph} and return object of type T.
 */
trait NodeToValueConverter[T] extends (NodeFromGraph => T) {
  def option =
    NodeBagConverter[Option[T]]( _ match {
      case NodeBag( Nil, _ ) => None
      case NodeBag( List( n ), g ) => Some( this( n(g) ) )
      case x => throw new RdfTraversalException( "Yielded multiple RDF nodes: " + x )
    } )
  
  def default( defaultValue: T ) = 
    NodeBagConverter[T]( bag => option( bag ) getOrElse defaultValue )
    
  def bag = 
    NodeBagConverter[NodeBag]( b => NodeBag( b.nodesFromGraph.map { Node from this( _ ) }, b.graph ) )
  
  def iterable = 
    NodeBagConverter[Iterable[T]]( _.nodesFromGraph.map { this( _ ) } )

  def set = 
    NodeBagConverter[Set[T]]( bag => Set.empty ++ bag.nodesFromGraph.map { this( _ ) } )
}

/**
 * A node-to-bag converter takes a node from graph and returns a bag of nodes from the same graph. 
 */
trait NodeToBagConverter extends (NodeFromGraph => NodeBag)

/**
 * Bag converters take a node bag and return object of type T (which, in turn, may also be a node bag).
 */
trait NodeBagConverter[T] extends (NodeBag => T)

object NodeBagConverter {
  def apply[T]( fn: NodeBag => T ) = new NodeBagConverter[T] {
    def apply( n: NodeBag ) = fn( n )
  }
}

/**
 * A node bag converter that creates a new bag in the same graph,
 * with only those nodes that satisfy given filter function.
 */
class NodeFilter( ffn: NodeFromGraph => Boolean ) 
extends NodeBagConverter[NodeBag] {
  def apply( bag: NodeBag ) = NodeBag( bag.nodes filter{ n => ffn( n(bag.graph) ) }, bag.graph ) 
}

/**
 * Filter factory object.
 */
object having {
  def apply( pred: UriRef ) = new NodeFilter( _ match {
    case gn: GraphNode => !(gn/pred).isEmpty
    case _ => false
  } )
  
  /**
   * Creates a NodeFilter which leaves only those nodes that 
   * have ALL of the given assignments in the graph.
   */
  def apply( assignments: Pair[UriRef, Any]* ) = 
    new NodeFilter( _ match {
      case gn: GraphNode => 
        assignments.map{ assignment => gn has assignment }.foldLeft(true){_&&_}
      case _ => false
    } )
}

object where {
  def apply( clause: NodeFromGraph => Boolean ) = new NodeFilter( clause )
}

trait GraphNodeConverter[T] extends NodeToValueConverter[T] {
  def apply( nfg: NodeFromGraph ) = nfg match {
    case gn: GraphNode => convertGraphNode( gn )
    case x => throw new RdfTraversalException( "Expected a graph node, not " + x ) 
  }
  
  def convertGraphNode( gn: GraphNode ): T
}

trait LiteralConverter[T] extends NodeToValueConverter[T] {
  def apply( nfg: NodeFromGraph ) = nfg match {
    case l: Literal => convertLiteral( l )
    case x => throw new RdfTraversalException( "Expected a literal, not " + x ) 
  }
  
  def convertLiteral( l: Literal ): T
}

class SimpleNodeConverter[T]( fn: NodeFromGraph => T ) extends NodeToValueConverter[T] {
  override def apply( n: NodeFromGraph ) = fn( n )
}

/**
 * Converts a literal to an object based on the type URI and a conversion function fn.
 * If the node is a typed literal with the specified type URI, the conversion function is applied.
 * If the node is a plain literal, the conversion function is applied. If the conversion function
 * throws an exception, the exception is ignored and an RDF traversal exception is thrown.
 * In all other cases, an RDF traversal exception is thrown.
 */
class TypeNodeConverter[T]( typename: String, domain: UriRef => Boolean, fn: String => T ) extends LiteralConverter[T] {
  private[this] def throwException( o: Any ) =
    throw new RdfTraversalException( "Not " + typename + ": " + o )
  
  override def convertLiteral( l: Literal ) = l match {
    case TypedLiteral( lf, typeUri ) if domain(typeUri) => fn( lf )
    case PlainLiteral( lf, _ ) => try { fn(lf) } catch { case e => throwException( l ) }
    case _ => throwException( l )
  }
  
  def pfn( f: T => Boolean ): Literal => Boolean = { l: Literal => f( convertLiteral(l) ) }
}

object NodeConverter {
  implicit def toNodeBagConverter( convert: NodeToBagConverter ) =
    NodeBagConverter[NodeBag]( sourceBag => 
      new NodeBag( sourceBag.nodesFromGraph flatMap { convert( _ ).nodes }, sourceBag.graph )
    )

  def multitypeNodeConverter[T]( typename: String, typeUriSet: Set[UriRef], fn: String => T ) =
    new TypeNodeConverter( typename, { t: UriRef => typeUriSet contains t }, fn )

  def typeNodeConverter[T]( typename: String, typeUri: UriRef, fn: String => T ): TypeNodeConverter[T] =
    multitypeNodeConverter[T]( typename, Set( typeUri ), fn )

  /**
   * Bag converter filters out all duplicate nodes in bag.
   */
  val distinct =
    NodeBagConverter[NodeBag]( b => NodeBag( b.nodes.distinct, b.graph ) )
  
  /**
   * Constructs a node-to-value converter which yields string value for plain literals
   * or literals typed as string.
   * Converter throws RdfTraversalException for non-literals, and also typed literals
   * with type other than {XSD.string}.
   * @see asLexic
   */
  implicit val asString = typeNodeConverter[String]( "a string", XSD.string, { x: String => x } )
  
  /**
   * Constructs a node-to-value converter yielding a lexical form for given literal.
   * Converter throws RdfTraversalException if given node is not a literal.
   */
  val asLexic = new SimpleNodeConverter[String]( {
    case l: Literal => l.lexicalForm
    case x => throw new RdfTraversalException( "Not a literal: " + x )
  } )

  /**
   * Converter for the lexical form for plain literals with given language tag.
   * Converter throws RdfTraversalException if given node is not a plain literal with given tag.
   * @throws IllegalArgumentException if given tag is null.
   */
  def asStringIn( tag: LangTag ) = {
    require( tag != null )
    new SimpleNodeConverter[String]( {
      case PlainLiteral( lf, Some(`tag`) ) => lf
      case x => throw new RdfTraversalException( "Not a string with lang tag " + tag + ": " + x )
    } )
  }

  /**
   * Converter for the option of lexical form for a plain literal with given language tag in bag.
   * Converter throws RdfTraversalException if there is more than one plain literal with given tag in bag.
   * @throws IllegalArgumentException if given tag is null.
   */
  def lexicIn( tag: LangTag ) = {
    require( tag != null )
    NodeBagConverter[Option[String]]{
      _.nodes flatMap {
        case PlainLiteral( lf, Some(`tag`) ) => List( lf )
        case _ => Nil
      } match {
        case List( one ) => Some( one )
        case Nil => None
        case x => throw new RdfTraversalException( "Multiple plain literals with lang tag " + tag + ": " + x )
      }
    }
  }

  implicit val asNode = new SimpleNodeConverter[Node]( _.node )
  implicit val asSubjectNode = new SimpleNodeConverter[SubjectNode]( _.node.asInstanceOf[SubjectNode] )
  
  implicit val asGraphNode = new GraphNodeConverter[GraphNode]{
    def convertGraphNode( gn: GraphNode ) = gn
  }
  
  def asRdfList[T]( nc: NodeToValueConverter[T] ) = new GraphNodeConverter[RdfList[T]]{
    def convertGraphNode( gn: GraphNode ) = GraphList( gn, nc )
  }
  
  implicit val asInt: TypeNodeConverter[Int] = 
    multitypeNodeConverter[Int]( "an int", Set( XSD.int, XSD.integer ), _.toInt )
  implicit val asBigInt: TypeNodeConverter[BigInt] =
    multitypeNodeConverter[BigInt]( "a big integer", Set( XSD.int, XSD.integer ), BigInt(_) )
  implicit val asBigDecimal =
    typeNodeConverter[BigDecimal]( "a big decimal", XSD.decimal, BigDecimal(_) )
  implicit val asFloat: TypeNodeConverter[Float] =
    typeNodeConverter[Float]( "a float", XSD.float, _.toFloat )
  implicit val asDouble: TypeNodeConverter[Double] = 
    typeNodeConverter[Double]( "a double", XSD.double, _.toDouble )
  implicit val asBoolean: TypeNodeConverter[Boolean] = 
    typeNodeConverter[Boolean]( "a boolean", XSD.boolean, _.toBoolean )
  implicit val asLocalDate = typeNodeConverter[LocalDate](
    "a date", XSD.date, IsoFormat.parseDateTime( _ ).toLocalDate
  )
  implicit val asDateTime = typeNodeConverter[DateTime](
    "date and time", XSD.dateTime, IsoFormat.parseDateTime( _ )
  )
}
