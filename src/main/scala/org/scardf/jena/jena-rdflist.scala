package org.scardf.jena

import org.scardf._
import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.util.iterator.ExtendedIterator

case class JenaRdfList( jlist: RDFList ) extends RdfList[Node] {
  def first = Jena.node( jlist.getHead )

  def rest = JenaRdfList( jlist.getTail )
  
  def node = Jena.subjectNode( jlist )
  
  override def foreach[U]( f: Node => U ): Unit =
    new JenaExtendedIterator( jlist.iterator ).foreach( f )
}

class JenaExtendedIterator( ei: ExtendedIterator[RDFNode] ) extends Iterator[Node] {
  def hasNext = ei.hasNext
  def next = Jena node ei.next
}
