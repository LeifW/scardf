package net.croz.scardf

import com.hp.hpl.jena.rdf.model._

case class Stmt( jStatement: Statement, rmodel: Model ) {
  val s: Res = rmodel getRes jStatement.getSubject
  val p: Prop = rmodel getProp jStatement.getPredicate
  val o: Node = Node( jStatement.getObject )
  
  def ? : Boolean = rmodel contains jStatement
}

object Stmt {
  def apply( s: Res, p: Prop, o: Any ): Stmt =
    apply( s.model.jModel.createStatement( s.jResource, p.jProperty, (Node from o).jNode ) )

  def apply( js: Statement ): Stmt = Model( js.getModel ).getStatement( js )
}

class RichStmtIterator( jIterator: StmtIterator ) extends Iterator[Stmt] {
  override def hasNext = jIterator.hasNext
  override def next = Stmt( jIterator.next.asInstanceOf[Statement] )
}
 