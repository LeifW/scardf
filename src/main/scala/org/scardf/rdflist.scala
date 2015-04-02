package org.scardf

import NodeConverter._
import scala.language.postfixOps

trait RdfList[E] extends Traversable[E] {
  def first: E
  def rest: RdfList[_<:E]
  def node: SubjectNode
}

final object RdfNil extends RdfList[Nothing] {
  override def isEmpty = true
  override def first: Nothing = throw new NoSuchElementException("first of empty list")
  override def rest: Nothing = throw new NoSuchElementException("rest of empty list")
  def node = RDF.nil
  override def foreach[U](f: Nothing => U): Unit = throw new NoSuchElementException("foreach on empty list")
}

case class GraphList[E]( gn: GraphNode, nc: NodeToValueConverter[E] ) extends RdfList[E] {
  def first = ( gn/RDF.first/nc.option ).getOrElse(
    throw new NoSuchElementException("first of empty list")
  )

  def rest = if ( gn.node == RDF.nil ) RdfNil else GraphList( gn/RDF.rest/asGraphNode, nc )
  
  def node = gn.node

  override def foreach[U](f: E => U): Unit = {
    var currentGNode = gn
    while ( currentGNode/RDF.first/#? ) {
      val e = currentGNode/RDF.first/nc
      f(e)
      currentGNode = currentGNode/RDF.rest/asGraphNode
    }
  }
}
