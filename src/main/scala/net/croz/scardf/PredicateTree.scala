package net.croz.scardf

object PredicateTree {
  
  val empty = new PredicateTree( Map() )
  
  def apply() = empty
  def apply( pairs: Pair[Prop, PredicateTree]* ) = new PredicateTree( Map( pairs: _* ) )
  def apply( p: Prop ): PredicateTree = toPredicateTree( p )
  def apply( pp: PropPath ): PredicateTree = toPredicateTree( pp )
  def apply( head: PredicateConstruct, tail: PredicateConstruct* ): PredicateTree =
    ( head :: tail.toList ).map{ from( _ ) }.foldLeft( empty )(_++_)

  def from( pc: PredicateConstruct ) = pc match {
    case t: PredicateTree => t
    case pp: PropPath => toPredicateTree( pp )
    case p: Prop => toPredicateTree( p )
  }
  
  //def strip( anchor: Res ) = 
  
  implicit def toPredicateTree( p: Prop ) = new PredicateTree( Map( p -> empty ) )
  
  implicit def toPredicateTree( pp: PropPath ): PredicateTree = prependTree( pp, empty )
  
  def prependTree( path: PropPath, endTree: PredicateTree ): PredicateTree = PredicateTree( 
    path.first -> (
      if ( path.size > 1 ) prependTree( path drop 1, endTree )
      else endTree
    )
  )
}

/**
 * A predicate tree is a mapping between URI references and other (possibly empty) predicate trees.
 * Should not contain itself, either directly or through contained predicate trees,
 * as this would lead to infinite loops in its methods.
 */
case class PredicateTree( branches: Map[Prop, PredicateTree] ) extends PredicateConstruct {
  
  def ++( other: PredicateTree ): PredicateTree = new PredicateTree( {
    val mergedMap = scala.collection.mutable.Map[Prop, PredicateTree]()
    mergedMap ++= branches
    for ( (predicate, subtree) <- other.branches )
      (branches get predicate) match {
        case Some( existingTree ) => mergedMap( predicate ) = existingTree ++ subtree
        case None => mergedMap( predicate ) = subtree
      }
    Map.empty ++ mergedMap
  } )
  
  def --( other: PredicateTree ): PredicateTree = new PredicateTree( {
    val remainMap = scala.collection.mutable.Map[Prop, PredicateTree]()
    remainMap ++= branches
    for ( (predicate, subtree) <- other.branches )
      (branches get predicate) match {
        case Some( PredicateTree.empty ) => remainMap removeKey predicate
        case Some( existingTree ) => remainMap( predicate ) = existingTree -- subtree
        case None => None
      }
    Map.empty ++ remainMap
  } )
  
  def subtree( path: PropPath ): PredicateTree = 
    if ( path.isEmpty ) this
    else branches( path.first ).subtree( path drop 1 )
  
  def subMerge( path: PropPath, other: PredicateTree ) =
    //TODO FIX! this does not return the whole graph!
    subtree( path ) ++ other

  /**
   * Given a predicate tree T and an RDF graph G with a node N, a subgraph may be constructed with
   * all triples from G in the form of (N, P, X), where P is any URI reference in the domain of T,
   * and then by adding more statements recursivly for each X with a predicate tree mapped to P in T. 
   */
  def growFrom( r: Res ) = growIn( r, new Model )
  private def growIn( root: Res, m: Model ): Res = {
    val seed = root in m
    for ( (predicate, subtree) <- branches ) {
      val newValues = root/predicate map { n: Node => n match {
        case res: Res => subtree.growIn( res, m )
        case n => n
      } }
      newValues foreach { a: Any => m add seed( predicate -> a ) }
    }
    seed
  }
  
  def growTemplateFrom( anchor: Res ) = {
    val m = new Model()
    val seed = anchor in m
    for ( (predicate, subtree) <- branches )
      m add seed( predicate -> subtree.growTemplateIn( m ) )
    seed
  }
  
  /**
   * Template graph is an RDF graph with only blank and literal nodes, 
   * and having one blank node singled out as an anchor.
   */
  def growTemplate = growTemplateIn( new Model )
  
  private def growTemplateIn( m: Model ): Res = {
    val seed = m.getAnon
    for ( (predicate, subtree) <- branches )
      m add seed( predicate -> subtree.growTemplateIn( m ) )
    seed
  }
  
  def isEmpty = branches.isEmpty
  def size = branches.size
  
  override def toString = {
    val bStrs = for ( branch <- branches ) yield {
      val subtree = branch._2
      branch._1.uri + (
        if ( subtree.isEmpty ) "" 
        else if ( subtree.size == 1 ) " - " + subtree
        else " -< " + subtree + " >"
      )
    }
    bStrs.mkString( "", "; ", "" )
  }
}
