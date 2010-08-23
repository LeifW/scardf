package org.scardf

case class RdfTriple( subj: SubjectNode, pred: UriRef, obj: Node )
{
  lazy val rend = subj.rend + " " + pred.rend + " " + obj.rend + " ."

  lazy val hasBlankNode = this match {
    case RdfTriple( b: Blank, _, _ ) => true
    case RdfTriple( _, _, b: Blank ) => true
    case _ => false
  }
}

case class SubPredPair( sub: SubjectNode, pred: UriRef ) {
  def ->( twig: Twig ) = Branch( sub, pred -> twig ) 
}
