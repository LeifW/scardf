package org.scardf

import NodeConverter._

trait RdfList[E] extends Iterable[E] {
  def first: E
  def rest: RdfList[_<:E]
  def node: SubjectNode
}

final object RdfNil extends RdfList[Nothing] {
  override def isEmpty = true
  def first: Nothing = throw new NoSuchElementException("first of empty list")
  def rest: Nothing = throw new NoSuchElementException("rest of empty list")
  def node = RDF.nil
  def elements = new Iterator[Nothing]() {
    def hasNext = false
    def next = throw new NoSuchElementException("next on empty list")
  }
}

case class GraphList[E]( gn: GraphNode, nc: NodeToValueConverter[E] ) extends RdfList[E] {
  def first = ( gn/RDF.first/nc.option ).getOrElse(
    throw new NoSuchElementException("first of empty list")
  )

  def rest = if ( gn.node == RDF.nil ) RdfNil else GraphList( gn/RDF.rest/asGraphNode, nc )
  
  def node = gn.node
  
  def elements = new Iterator[E]() {
    var currentGNode = gn
    def hasNext = currentGNode/RDF.first/#?
    def next = {
      val e = currentGNode/RDF.first/nc
      currentGNode = currentGNode/RDF.rest/asGraphNode
      e
    }
  }
}
