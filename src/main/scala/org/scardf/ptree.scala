package org.scardf

trait PredicateConstruct

object PredicateTree {
  
  val empty = new PredicateTree( Map(), Map() )
  
  def apply( pairs: Pair[UriRef, PredicateTree]* )( opairs: Pair[UriRef, PredicateTree]* ) = 
    new PredicateTree( Map( pairs: _* ), Map( opairs: _* ) )

  def apply( p: UriRef ): PredicateTree = toPredicateTree( p )
  def apply( subtrees: PredicateTree* ): PredicateTree = subtrees.foldLeft( empty ){_++_}
  
  def opt( opairs: Pair[UriRef, PredicateTree]* ) = new PredicateTree( Map(), Map( opairs: _* ) )
  
  implicit def toPredicateTree( p: UriRef ) = new PredicateTree( Map( p -> empty ), Map() )
}

/**
 * A predicate tree is a pair of mappings between URI references and other (possibly empty) predicate trees.
 * One mapping contains required URI references, the other contains optional URI references.
 */
class PredicateTree( val branches: Map[UriRef, PredicateTree], val optionals: Map[UriRef, PredicateTree] )
extends PredicateConstruct {

  /**
   * Returns newly created tree constructed by merging this and given predicate trees.
   */
  def ++( other: PredicateTree ): PredicateTree = new PredicateTree(
    merge( branches, other.branches ), merge( optionals, other.optionals )
  )

  private[this] def merge( m1: Map[UriRef, PredicateTree], m2: Map[UriRef, PredicateTree] ) = {
    val mergedMap = scala.collection.mutable.Map[UriRef, PredicateTree]()
    mergedMap ++= m1
    for ( (predicate, subtree) <- m2 )
      (m1 get predicate) match {
        case Some( existingTree ) => mergedMap( predicate ) = existingTree ++ subtree
        case None => mergedMap( predicate ) = subtree
      }
    Map.empty ++ mergedMap
  }

  /**
   * Given a predicate tree T and an RDF graph G with a node N, a subgraph may be constructed with
   * all triples from G in the form of (N, P, X), where P is any URI reference in the domain of T,
   * and then by adding more statements recursively for each X with a predicate tree mapped to P in T. 
   */
  def growNew( root: GraphNode ): GraphNode = grow( root, new MutableSetGraph() )

  /**
   * Given a predicate tree T and an RDF graph G with a node N, a subgraph may be constructed with
   * all triples from G in the form of (N, P, X), where P is any URI reference in the domain of T,
   * and then by adding more statements recursively for each X with a predicate tree mapped to P in T. 
   * @throws RdfTraversalException if required predicate has no value in any required node
   */
  def grow( root: GraphNode, outGraph: Graph ): GraphNode = {
    var g = outGraph
    for ( (predicate, subtree) <- branches ++ optionals ) {
      val bag = root/predicate
      if ( bag.isEmpty && ( branches contains predicate ) )
        throw new RdfTraversalException(
          "Required predicate " + predicate + " not found for " + root.node + " in " + root.graph
        );
      val newValues: Seq[NodeFromGraph] = bag.nodes map {
        case res: SubjectNode => subtree.grow( bag.graph/res, g )
        case n: NodeFromGraph => n
      }
      newValues foreach { a: Any => 
        a match {
	      case sn: GraphNode => g ++= sn.graph //TODO use mutable outgraph and skip this?
	      case _ =>
	    }
	    g += RdfTriple( root.node, predicate, Node from a )
      }
    }
    g/root.node
  }
  
  /**
   * Creates a copy of the given RDF list in target graph,
   * with every list item expanded through the predicate tree.
   * @param rootl 
   * Node must be an RDF list, i.e. it must either be a RDF:nil,
   * or a subject node with a single RDF.first property and a RDF.rest property, 
   * where the value of the RDF.rest property is a RDF list itself.
   */
  def spanRdfList( rootl: GraphNode, target: Graph ) = {
    import NodeConverter.asGraphNode
    var g = target
    var l = rootl
    while ( l.node != RDF.nil ) {
      val head = l/RDF.first/asGraphNode
      val tail = l/RDF.rest/asGraphNode
      g += RdfTriple( l.node, RDF.first, head.node )
      grow( head, g )
      g += RdfTriple( l.node, RDF.rest, tail.node )
      l = tail
    }
    g/rootl.node
  }
  
//  def growTemplateFrom( anchor: GraphNode ) = {
//    val seed = anchor
//    for ( (predicate, subtree) <- branches )
//      m add seed( predicate -> subtree.growTemplateIn( m ) )
//    seed
//  }
  
  /**
   * Template graph is an RDF graph with only blank and literal nodes, 
   * and having one blank node singled out as an anchor.
   */
//  def growTemplate = growTemplateIn( Graph() )
//  
//  private def growTemplateIn( g: Graph ): GraphNode = {
//    val seed = g.getAnon
//    for ( (predicate, subtree) <- branches )
//      m add seed( predicate -> subtree.growTemplateIn( m ) )
//    seed
//  }
  
  def isEmpty = branches.isEmpty && optionals.isEmpty
  def size = branches.size + optionals.size
  
  override lazy val hashCode = 41 * (41 + branches.hashCode) + optionals.hashCode
  
  override def equals( other: Any ): Boolean = other match {
    case that: PredicateTree => 
      this.branches == that.branches && this.optionals == that.optionals
    case _ => false
  }
  
  def canEqual( other: Any ) = other.isInstanceOf[PredicateTree]

  import scala.collection.mutable.ArrayBuffer
  
  def buildPatternBlock( anchor: TermPlace ): PatternBlock = {
    val reqBlocks = new ArrayBuffer[PatternBlock]()
    val optBlocks = new ArrayBuffer[PatternBlock]()
    for ( (predicate, subtree) <- branches ) {
      val subblock = subtree.buildPatternBlock( anchor, predicate )
      reqBlocks += subblock
    }
    for ( (predicate, subtree) <- optionals ) {
      val subblock = subtree.buildPatternBlock( anchor, predicate )
      optBlocks += subblock
    }
    PatternBlock( List(), reqBlocks.toList, optBlocks.toList )
  }
  
  def buildPatternBlock( term: TermPlace, pred: UriRef ): PatternBlock = {
    val tts = new ArrayBuffer[TemplateTriple]()
    val reqBlocks = new ArrayBuffer[PatternBlock]()
    val optBlocks = new ArrayBuffer[PatternBlock]()
    val v = QVar()
    tts += TemplateTriple( term, pred, v )
    for ( (predicate, subtree) <- branches ) {
      val subblock = subtree.buildPatternBlock( v, predicate )
      reqBlocks += subblock
    }
    for ( (predicate, subtree) <- optionals ) {
      val subblock = subtree.buildPatternBlock( v, predicate )
      optBlocks += subblock
    }
    PatternBlock( tts.toList, reqBlocks.toList, optBlocks.toList )
  }

  private[this] def segmentStrings( segments: Map[UriRef, PredicateTree], qualifier: String ) = 
    ( for ( branch <- segments ) yield {
      val subtree = branch._2
      branch._1.uri + (
        if ( subtree.isEmpty ) qualifier 
        else if ( subtree.size == 1 ) qualifier + "~" + subtree
        else qualifier + "~( " + subtree + " )"
      )
    } ).toList

  override def toString = 
    ( segmentStrings( branches, "" ) ::: segmentStrings( optionals, "?" ) ).mkString( "", "; ", "" )
}

case class PatternBlock( tt: List[TemplateTriple], rb: List[PatternBlock], ob: List[PatternBlock] ) {
  def rend: String = 
    tt.map{ _.rend + " " }.mkString( " " )+
    rb.map{ _.rend }.mkString( " " ) +
    ( ob.map{ pb => " OPTIONAL { " + pb.rend + " } " }.foldLeft(""){_+_} )
  
  def allTriples: List[TemplateTriple] = 
    tt ::: (rb ::: ob).map( _.allTriples ).foldLeft(List[TemplateTriple]()){ _:::_ }
  
  def construct = "CONSTRUCT { " + allTriples.map{ _.rend + " " }.foldLeft(""){_+_} +
    " } WHERE { " + rend + " }"
}
