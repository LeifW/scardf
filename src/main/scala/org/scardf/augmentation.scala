 package org.scardf

import NodeConverter._
import org.joda.time._
import org.joda.time.format.DateTimeFormat

object Augment {
  def triples( pf: PartialFunction[Triple, Iterable[Branch]] ) =
    new PFAugmenter( pf )
  
  def fromBag( bagFn: Graph => NodeBag )( twigFn: PartialFunction[NodeFromGraph, Iterable[Twig]] ) =
    new NodebagAugmenter( bagFn, twigFn )

  def add( addFn: GraphNode => Branch ) = new AugPreparator().add( addFn )
}

class AugPreparator() {
  var addFunctions: List[GraphNode => Branch] = Nil

  var filterFn: Graph => NodeBag = { _/- } // default: all subjects in graph
  
  def add( branchFn: GraphNode => Branch ) = { addFunctions ::= branchFn; this }
  
  def forEach( fn: Graph => NodeBag ) = { filterFn = fn; this }
  
  def on( g: Graph ) = new SnBagAugmenter( filterFn, addFunctions ) augmented g
}

trait Augmenter {
  def augmentations( g: Graph ): Iterable[Triple]
  
  def augmented( g: Graph ): Graph = g ++ augmentations( g )
}

class CompositeAugmenter( augs: Iterable[Augmenter] ) extends Augmenter {
  def augmentations( g: Graph ) = augs flatMap{ _.augmentations( g ) }
}

abstract class FindMakeAugmenter extends Augmenter {
  def findSubjects( g: Graph ): Set[GraphNode]

  def makeTriple( n: GraphNode ): Triple
  
  def augmentations( src: Graph ) = findSubjects( src ) map makeTriple
}

abstract class PredAugmenter( pred: UriRef ) extends Augmenter {
  def augment( nfg: NodeFromGraph ): Pair[UriRef, Node]
  
  def augmentations( g: Graph ) = g.triples filter {
    case Triple( _, `pred`, _ ) => true
    case _ => false
  } map { t => {
    val ( p, o ) = augment( t.obj(g) )
    Triple( t.subj, p, o ) 
  } }
}

class PFAugmenter( pf: PartialFunction[Triple, Iterable[Branch]] ) extends Augmenter {
  val returnNone: PartialFunction[Triple, Iterable[Branch]] = { case _ => List() }

  def augmentations( g: Graph ) = {
    val fn = pf orElse returnNone
    val result = collection.mutable.Set[Triple]()
    for ( t <- g.triples; b <- fn( t ) ) result ++= b.triples
    result
//TODO FIX shorter expr:   g.triples.flatMap{ x => ( pf orElse returnNone )( x ).triples }
  }
}

class NodebagAugmenter( bagFn: Graph => NodeBag, twigFn: PartialFunction[NodeFromGraph, Iterable[Twig]] )
extends Augmenter {
  val returnNone: PartialFunction[NodeFromGraph, Iterable[Twig]] = { case _ => Nil }

  def augmentations( g: Graph ) =
    bagFn( g ).nodesFromGraph.flatMap{ x => ( twigFn orElse returnNone )( x ).triples }
}

class SnBagAugmenter( bagFn: Graph => NodeBag, branchFns: List[GraphNode => Branch] ) 
extends Augmenter {
  def triples( gnode: GraphNode ): Set[Triple] = (
    branchFns map { fn: (GraphNode => Branch) => fn( gnode ).triples }
  ).foldLeft( Set[Triple]() ){ _ ++ _ } 

  override def augmentations( g: Graph ) =
    bagFn( g ).nodesFromGraph.flatMap{ nfg => triples( nfg.asInstanceOf[GraphNode] ) }
}

class DateFormatter( datePred: UriRef, dateLabelPred: UriRef, dateFormatPattern: String ) 
extends PredAugmenter(datePred) {
  val formatter = DateTimeFormat.forPattern( dateFormatPattern )
  def augment( nfg: NodeFromGraph ) = dateLabelPred -> Node.from( 
    formatter.print( asLocalDate(nfg) ) 
  )
}
