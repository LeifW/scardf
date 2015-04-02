package org.scardf

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import sys.error

object PredicateTree {
  val empty = new PredicateTree( Set.empty )
  def apply( items: PredicateBranch* ) = new PredicateTree( Set( items: _* ) )
}

object PredicateBranch {
  implicit def toPredBranch( p: UriRef ) =
    new PredicateBranch( p, true, true, PredicateTree.empty )
}

object AnyPredicate extends PredicateBranch( null, true, true, PredicateTree.empty )

case class PredicateBranch( predicate: UriRef, forward: Boolean, required: Boolean, pt: PredicateTree ) {

  def ? = PredicateBranch( predicate, forward, false, pt )
  def unary_- = PredicateBranch( predicate, !forward, required, pt )

  def ~( subtree: PredicateTree ): PredicateBranch = this~( subtree.pbs.toList: _* )
  
  def ~( subs: PredicateBranch* ): PredicateBranch = pt.pbs.toList match {
    case List() =>
      PredicateBranch( predicate, forward, required, PredicateTree( subs: _* ) )
    case List( pb ) => //TODO ?
      PredicateBranch( predicate, forward, required, PredicateTree( pb~( subs: _* ) ) )
    case x =>
      error( "Cannot continue branch " + this + " on " + x )
  }
  
  def buildPatternBlock( anchor: TermPlace ): PatternBlock = {
    val reqBlocks = new ArrayBuffer[PatternBlock]()
    val optBlocks = new ArrayBuffer[PatternBlock]()
    val v = QVar()
    val predTerm = if ( predicate == null ) QVar() else predicate
    val tt = if (forward) TemplateTriple( anchor, predTerm, v )
    		 else TemplateTriple( v, predTerm, anchor )
    for ( branch <- pt.pbs ) {
      val subblock = branch.buildPatternBlock( v )
      if (branch.required) reqBlocks += subblock
      else optBlocks += subblock
    }
    PatternBlock( List( tt ), reqBlocks.toList, optBlocks.toList )
  }
  
  def grow( root: GraphNode, outGraph: MutableGraph ) {
    val predTerm = if ( predicate == null ) Node else predicate
    val subjTerm = if (forward) root.node else Node
    val objTerm =  if (forward) Node else root.node
    val matchedTriples = root.graph.triplesLike( subjTerm, predTerm, objTerm )
    if (required && matchedTriples.isEmpty)
      error( this + " matched no " + (subjTerm, predTerm, objTerm) + " triples on " + root )
    outGraph ++= matchedTriples //TODO replace ++
    matchedTriples.map( t => if (forward) t.obj else t.subj ).foreach( n => n match {
      case sn: SubjectNode => pt.grow( root.graph/sn, outGraph )
      case _ =>
    } )
  }
  
  override def toString =
    ( if (forward) "" else "-") + 
    ( if (predicate == null) "*" else predicate.toString ) +
    ( if (required) "" else "?" ) +
    (pt.pbs.toList match {
      case Nil => ""
      case List( pb ) => "~" + pb.toString
      case _ => "~" + pt.toString
    })
}

class PredicateTree( val pbs: Set[PredicateBranch] ) {
  
  def buildPatternBlock( anchor: TermPlace ): PatternBlock = {
    val reqBlocks = new ArrayBuffer[PatternBlock]()
    val optBlocks = new ArrayBuffer[PatternBlock]()
    for ( item <- pbs ) {
      val subblock = item.buildPatternBlock( anchor )
      if (item.required) reqBlocks += subblock
      else optBlocks += subblock
    }
    PatternBlock( List(), reqBlocks.toList, optBlocks.toList )
  }
  
  def +( pt: PredicateTree ) = new PredicateTree( pbs ++ pt.pbs )
  
  /**
   * Given a predicate tree T and an RDF graph G with a node N, a subgraph may be constructed with
   * all triples from G in the form of (N, P, X), where P is any URI reference in the domain of T,
   * and then by adding more statements recursively for each X with a predicate tree mapped to P in T. 
   */
  def growNew( root: GraphNode ): GraphNode = {
    val out = new MutableSetGraph()
    grow( root, out )
    out/root.node
  }

  /**
   * Given a predicate tree T and an RDF graph G with a node N, a subgraph may be constructed with
   * all triples from G in the form of (N, P, X), where P is any URI reference in the domain of T,
   * and then by adding more statements recursively for each X with a predicate tree mapped to P in T. 
   * @throws RdfTraversalException if required predicate has no value in any required node
   */
  def grow( root: GraphNode, outGraph: MutableGraph ) {
    for ( branch <- pbs )
      branch.grow( root, outGraph )
  }
  
  override def toString =
    pbs.mkString( "{ ", ", ", " }" )
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
