package net.croz.scardf.build

abstract class Fetcher {

  def fetch( in: Res ): Res = {
    val resultModel = new Model
    if ( in.isRdfList )
      fetchList( in.asRdfList, resultModel )
    else
      fetch( in, resultModel )
  }

  def fetchList( inList: RdfList, resultModel: Model ) = {
    val rlist = new scala.collection.mutable.ListBuffer[Res]()
    for ( r <- inList )
      rlist += fetch( r.asRes, resultModel )
    RdfList( rlist: _* )( resultModel )
  }
  
  def fetch( one: Res, resultModel: Model ): Res
}

class TakeFetcher( takes: Collection[_] ) extends Fetcher {
  def fetch( one: Res, resultModel: Model ): Res = {
    Sparql take takes to resultModel from one
    one in resultModel
  }
}
