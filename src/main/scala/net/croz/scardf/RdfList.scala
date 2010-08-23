package net.croz.scardf

import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.RDFList

class RdfList( val jRdfList: RDFList, override val model: Model ) extends Res( jRdfList, model )
with scala.Seq[Node] with util.Logging
{
  def toNodeBag: NodeBag = new NodeBag( elements.toList )

  def jlist: List[RDFNode] = 
    scala.collection.JavaConversions.JListWrapper( jRdfList.asJavaList.asInstanceOf[java.util.List[RDFNode]] ).toList

  def length = jRdfList.size
  override def iterator: Iterator[Node] = jlist.map{ n: RDFNode => Node( n ) }.elements
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
