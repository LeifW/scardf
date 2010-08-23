package org.scardf

import NodeConverter._
import org.joda.time._
import org.joda.time.format.DateTimeFormat

object Augment {
  def triples( pf: PartialFunction[RdfTriple, Iterable[Branch]] ) =
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
  def augmentations( g: Graph ): Iterable[RdfTriple]
  
  def augmented( g: Graph ): Graph = g ++ augmentations( g )
}

class CompositeAugmenter( augs: Iterable[Augmenter] ) extends Augmenter {
  def augmentations( g: Graph ) = augs flatMap{ _.augmentations( g ) }
}

abstract class FindMakeAugmenter extends Augmenter {
  def findSubjects( g: Graph ): Set[GraphNode]

  def makeTriple( n: GraphNode ): RdfTriple
  
  def augmentations( src: Graph ) = findSubjects( src ) map makeTriple
}

abstract class PredAugmenter( pred: UriRef ) extends Augmenter {
  def augment( nfg: NodeFromGraph ): Pair[UriRef, Node]
  
  def augmentations( g: Graph ) = g.triples filter {
    case RdfTriple( _, `pred`, _ ) => true
    case _ => false
  } map { t => {
    val ( p, o ) = augment( t.obj(g) )
    RdfTriple( t.subj, p, o ) 
  } }
}

class PFAugmenter( pf: PartialFunction[RdfTriple, Iterable[Branch]] ) extends Augmenter {
  val returnNil: PartialFunction[RdfTriple, Iterable[Branch]] = { case _ => List() }

  def augmentations( g: Graph ) = {
    val fn = pf orElse returnNil
    val result = collection.mutable.Set[RdfTriple]()
    for ( t <- g.triples; b <- fn( t ) ) result ++= b.triples
    result
//TODO FIX shorter expr:   g.triples.flatMap{ x => ( pf orElse returnNone )( x ).triples }
  }
}

class NodebagAugmenter( bagFn: Graph => NodeBag, twigFn: PartialFunction[NodeFromGraph, Iterable[Twig]] )
extends Augmenter {
  val returnNil: PartialFunction[NodeFromGraph, Iterable[Twig]] = { case _ => Nil }

  def augmentations( g: Graph ) =
    bagFn( g ).nodesFromGraph.flatMap{ x => ( twigFn orElse returnNil )( x ).triples }
}

class SnBagAugmenter( bagFn: Graph => NodeBag, branchFns: List[GraphNode => Branch] ) 
extends Augmenter {
  def triples( gnode: GraphNode ): Set[RdfTriple] = (
    branchFns map { fn: (GraphNode => Branch) => fn( gnode ).triples }
  ).foldLeft( Set[RdfTriple]() ){ _ ++ _ } 

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
