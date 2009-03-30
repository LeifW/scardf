package net.croz.scardf

import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.RDFList
import scala.collection.jcl.Conversions.convertList

class RdfList( val jRdfList: RDFList, override val model: Model ) extends Res( jRdfList, model )
with scala.Seq[Node] with util.Logging
{
  def toNodeBag: NodeBag = new NodeBag( elements.toList )

  def jlist: List[RDFNode] = 
    convertList( jRdfList.asJavaList.asInstanceOf[java.util.List[RDFNode]] ).toList

  def length = jRdfList.size
  def elements: Iterator[Node] = jlist.map{ n: RDFNode => Node( n ) }.elements
  def apply( i: Int ) = Node( jRdfList.get(i) )
}

object RdfList {
  def from( l: RDFList ): RdfList = Model( l.getModel ) getRdfList l

  def from( c: Collection[Any] )( implicit model: Model ): RdfList = 
    apply( c.toArray: _* )( model )
  
  def apply( nodes: Any* )( implicit model: Model ) = {
    val jNodes = nodes map { Node from _ jNode }
    val jList = model createList jNodes.toArray
    model getRdfList jList
  }
}

/**
 * Converts single node to a list.
 */
object asRdfList extends NodeConverter[RdfList] ( _.asRdfList )

/**
 * Converts single node to a new bag containing its list elements.
 */
object asRdfListBag extends NodeConverter[NodeBag] ( _.asRdfList.toNodeBag )
