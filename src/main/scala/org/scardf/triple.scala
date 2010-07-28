package org.scardf

case class Triple( subj: SubjectNode, pred: UriRef, obj: Node )
extends Tuple3[SubjectNode, UriRef, Node]( subj, pred, obj )
{
  lazy val rend = subj.rend + " " + pred.rend + " " + obj.rend + " ."

  lazy val hasBlankNode = this match {
    case Triple( b: Blank, _, _ ) => true
    case Triple( _, _, b: Blank ) => true
    case _ => false
  }
}

case class SubPredPair( sub: SubjectNode, pred: UriRef ) {
  def ->( twig: Twig ) = Branch( sub, pred -> twig ) 
}
