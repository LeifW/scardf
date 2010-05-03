package org.scardf

abstract class Twig {
  def triples: Set[Triple]
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
  override def triples: Set[Triple] = {
    val tsetseq: Seq[Set[Triple]] = for ( a <- assignments ) yield
      a._2.values.map{ Triple( root, a._1, _ ) } ++ a._2.triples
    if ( tsetseq.isEmpty ) Set[Triple]() else tsetseq reduceLeft{ _ ++ _ }
  }
  
  override def values = Set( root )
  
  def toGraph = Graph( triples )

  def rend = root + ": " + assignments
}

object Branch {
  
  implicit def toBranch( t: Triple ) = Branch( t.sub, t.pred -> ObjSet( t.obj ) )
  
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
}

object ObjSet {
  val empty = Set[Triple]()
  def apply( nodes: Node* ) = new ObjSet( Set( nodes: _* ) )
}

case class ListBranch( l: List[_] ) extends Twig {
  private val b = Blank()
  override val triples: Set[Triple] = l match {
    case Nil => Set()
    case List( one ) => Set( Triple( b, RDF.first, Node from one ), Triple( b, RDF.rest, RDF.nil ) )
    case first :: rest => 
      val sublist = ListBranch( rest )
      sublist.triples + Triple( b, RDF.first, Node from first ) + Triple( b, RDF.rest, sublist.b )
  }
  override def values = Set( b )
}