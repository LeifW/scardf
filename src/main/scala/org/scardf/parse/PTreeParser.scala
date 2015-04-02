package org.scardf.parse

import util.parsing.combinator.JavaTokenParsers
import org.scardf._
import scala.language.postfixOps

class PTreeParser extends JavaTokenParsers {
  def doc: Parser[PTreeDoc] = prologue ~ defs ^^ {
    case p ~ d => PTreeDoc( p, d )
  }
  def defs: Parser[List[PTDef]] = (repsep(fullBranch, ",") | (ptdef*)) ^^ {
    //case pte: PTreeExpr => List( PTDef( "", pte ) ) //fruitless type test: a value of type List[Object] cannot also be a org.scardf.parse.PTreeExpr
    case l: List[PTDef @unchecked] => l
  }
  def prologue: Parser[List[PrefixDef]] = prefixDef*
  def prefixDef: Parser[PrefixDef] = "@prefix" ~> prefix ~ ":" ~ urirefStr <~ "." ^^ {
    case p ~ _ ~ u => PrefixDef( p, u )
  }
  def prefix: Parser[String] = ident
  def urirefStr: Parser[String] = "<" ~> """([^<>"{}|^`\\])*""".r <~ ">"
  def ptdef: Parser[PTDef] = varname ~ ":=" ~ ptree ~ "." ^^ {
    case v ~ _ ~ e ~ _ => PTDef( v, e )
  }
  def varname = ident
  def ptree: Parser[PTreeExpr] = repsep(branch, ",") ^^ PTreeExpr
  def branch: Parser[BranchExpr] = fullBranch | varref
  def varref: Parser[BranchExpr] = varname ^^ { VarRef( _ ) }
  def fullBranch: Parser[BranchExpr] = opt("-") ~ predref ~ opt("?") ~ opt( "~" ~> subbranches ) ^^ {
    case backMark ~ pr ~ optMark ~ subs => FullBranch( pr, backMark == None, optMark == None, PTreeExpr(subs getOrElse Nil))
  }
  def subbranches: Parser[List[BranchExpr]] = singleBranch | branchList
  def singleBranch: Parser[List[BranchExpr]] = branch ^^ {List(_)}
  def branchList: Parser[List[BranchExpr]] = "(" ~> repsep(branch, ",") <~ ")"
  def predref: Parser[PredRef] = anyref | uriref | shortref
  def uriref: Parser[PredRef] = urirefStr ^^ {Uriref(_)}
  def anyref: Parser[PredRef] = "*" ^^ {_ => AnyRef}
  def shortref: Parser[Localref] = prefix ~ ":" ~ localref ^^ { case a ~_~ b => Localref( a, b ) }
  def localref: Parser[String] = "[^=\\?\\s]+".r
}

case class PTreeDoc( prefixes: List[PrefixDef], ptdefs: List[PTDef] )
case class PrefixDef( pref: String, uri: String )
case class PTDef( varname: String, ptExpr: PTreeExpr )
case class PTreeExpr( bex: List[BranchExpr] )
abstract class BranchExpr
case class VarRef( varname: String ) extends BranchExpr
case class FullBranch( pr: PredRef, fw: Boolean, rq: Boolean, pt: PTreeExpr ) extends BranchExpr
abstract class PredRef
case object AnyRef extends PredRef
case class Uriref( ref: String ) extends PredRef
case class Localref( p: String, localpart: String ) extends PredRef

object PtParse {
  def main( args: Array[String] ) {
    val input = """
@prefix b: <http://b.eg/> .

A := -*?, b:three .
B := b:one ~ b:two? ~ A . 
C := <http://example.org> ~ b:bla? ~ -b:bla . 
"""
    println( parse( input ) )
//    println( parse( "b:one ~ b:two?" ) )
  }
  
  def parse( input: String ) = {
    val p = new PTreeParser
    val ptc = new PtConstructor
    val doc = p.parseAll( p.doc, input )
    println(doc)
    ptc.construct( doc.get )
  }
}

class PtConstructor {
  var prefixMap = Map[String, String]()
  var varMap = Map[String, PredicateTree]()
  
  def construct( doc: PTreeDoc ): Map[String, PredicateTree] = {
    prefixMap = Map( doc.prefixes.map( p => p.pref -> p.uri ): _* )
    for ( d <- doc.ptdefs ) {
      varMap += d.varname -> construct( d.ptExpr )
    }
    varMap
  }
  
  def construct( pte: PTreeExpr ): PredicateTree = {
    PredicateTree( pte.bex.flatMap( construct( _ ) ): _* )
  }
  
  def construct( be: BranchExpr ): Seq[PredicateBranch] = be match {
    case VarRef( name ) => varMap(name).pbs.toSeq
    case FullBranch( predRef, fw, rq, pte ) => PredicateBranch( construct( predRef ), fw, rq, construct(pte) ) :: Nil
  }
  
  def construct( pr: PredRef ) = pr match {
    case AnyRef => null
    case Uriref( ref ) => UriRef( ref )
    case Localref( p, localpart ) => UriRef( prefixMap(p) + localpart ) 
  }
}
