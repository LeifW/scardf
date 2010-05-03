package org.scardf

case class Triple( sub: SubjectNode, pred: UriRef, obj: Node )
extends Tuple3[SubjectNode, UriRef, Node]( sub, pred, obj )
{
  lazy val rend = sub.rend + " " + pred.rend + " " + obj.rend + " ."
  
  lazy val hasBlankNode = this match {
    case Triple( b: Blank, _, _ ) => true
    case Triple( _, _, b: Blank ) => true
    case _ => false
  }
}

case class SubPredPair( sub: SubjectNode, pred: UriRef ) {
  def ->( twig: Twig ) = Branch( sub, pred -> twig ) 
}
