package org.scardf

abstract class Twig {
  def triples: Set[RdfTriple]
  def values: Set[Node]
}

object Twig {
  implicit def toTwig( a: Any ): Twig = a match {
	case b: Branch => b
    case os: ObjSet => os
    case l: List[_] => ListBranch( l )
	case x => ObjSet( Node from x )
  }
}

case class Branch( root: SubjectNode, assignments: Pair[ UriRef, Twig ]* ) extends Twig {
  override def triples: Set[RdfTriple] = {
    val tsetseq: Seq[Set[RdfTriple]] = for ( a <- assignments ) yield
      a._2.values.map{ RdfTriple( root, a._1, _ ) } ++ a._2.triples
    if ( tsetseq.isEmpty ) Set[RdfTriple]() else tsetseq reduceLeft{ _ ++ _ }
  }
  
  override def values = Set( root )
  
  /** Creates a new branch with the same root and some additional assignments */
  def ++( additional: Pair[ UriRef, Twig ]* ) =
    new Branch( root, assignments ++ additional: _* )
  
  def toGraph = Graph( triples )

  def rend = root + ": " + assignments
  
  override def toString = root + "-( " + assignments.mkString( ", " ) + " )"
}

object Branch {
  
  implicit def toBranch( t: RdfTriple ) = Branch( t.subj, t.pred -> ObjSet( t.obj ) )
  
  def of( root: SubjectNode, assignments: Seq[ Pair[ UriRef, Twig ] ] ) =
    Branch( root, assignments: _* )
  
  def make( root: SubjectNode, assignments: Pair[ UriRef, Any ]* ): Branch = {
    val branchSeq: Seq[ Pair[ UriRef, Twig ] ] = 
      for ( a <- assignments ) yield Pair( a._1, Twig.toTwig( a._2 ) )
    of( root, branchSeq )
  }
  
  def apply( assignments: Pair[ UriRef, Any ]* ): Branch = make( Blank(), assignments: _* )
}

class ObjSet( objs: Set[Node] ) extends Twig {
  override val triples = ObjSet.empty
  override val values = objs
  override def toString = objs.mkString( "{", ", ", "}" )
}

object ObjSet {
  val empty = Set[RdfTriple]()
  def apply( nodes: Node* ) = new ObjSet( Set( nodes: _* ) )
}

case class ListBranch( l: List[_] ) extends Twig {
  private val b = Blank()
  override val triples: Set[RdfTriple] = l match {
    case Nil => Set()
    case List( one ) => Set( RdfTriple( b, RDF.first, Node from one ), RdfTriple( b, RDF.rest, RDF.nil ) )
    case first :: rest => 
      val sublist = ListBranch( rest )
      sublist.triples + RdfTriple( b, RDF.first, Node from first ) + RdfTriple( b, RDF.rest, sublist.b )
  }
  override def values = Set( b )
}
