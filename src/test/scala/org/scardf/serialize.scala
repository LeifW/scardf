package org.scardf

import org.specs2.mutable._
import java.io.{CharArrayReader, CharArrayWriter, FileReader, StringReader}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.matcher.MatchResult

@RunWith(classOf[JUnitRunner])
object SerializerSpecs extends Specification {
  def parse( input: String, triples: Set[RdfTriple] ):MatchResult[Boolean] ={
    val parsedGraph = new jena.JenaGraph ++ new Serializator(NTriple).readFrom(new StringReader(input))
    val expectedGraph = new jena.JenaGraph ++ triples
     ( expectedGraph =~ parsedGraph ) must beTrue
  }
  def parse( input: String ):MatchResult[Boolean]= { parse( input, Set[RdfTriple]() ) }
  def parse( input: String, branch: Branch ):MatchResult[Boolean]= { parse( input, branch.triples ) }

  "Base graph serialization / deserialization mechanism" should {
    val a = UriRef("a")
    val b = UriRef("b")
    val c = UriRef("c")
    val b1 = new Blank("b1")
    val b2 = new Blank("b2")

    "parse an empty line" in parse( "\n" )
    
    "parse a comment" in parse( " # a comment\n" )
    "parse an all-URIref triple" in parse( "<a> <b> <c> .\n", a-b->c )
    "parse triple with blanks" in parse( "_:b1 <b> _:b2 .\n", b1-b->b2 )
    "parse triple with plain literals" in parse( "_:b1 <b> \"c\" .\n", b1 -b-> PlainLiteral("c") )
    "parse lang tags" in parse( "<a> <b> \"c\"@en .\n", a -b-> PlainLiteral("c", Some(LangTag("en")) ) )
    "parse typed literals" in parse( "<a> <b> \"c\"^^<a> .\n", a -b-> TypedLiteral("c", a ) )
    "parse documents with no eoln" in parse( "<a> <b> <c> .", a-b->c )

    "parse the official test file just like Jena's parser" in {
      val testFileLocation = "src/test/scala/org/scardf/test.nt"
      val baseG = new jena.JenaGraph ++ new Serializator(NTriple).readFrom( new FileReader( testFileLocation ) )
      val jenaG = new jena.JenaSerializator(NTriple).readFrom( new FileReader( testFileLocation ) )
      ( baseG =~ jenaG ) must beTrue
    }
    "be round-tripable" in {
      val g = new jena.JenaGraph ++ Doe.graph
      val s = new Serializator( NTriple )
      val w = new CharArrayWriter
      s.write(g, w)
      val r = new CharArrayReader( w.toCharArray )
      val gout = new jena.JenaGraph ++ s.readFrom(r)
      ( gout =~ g )must beTrue
    }
//    "pass scalacheck" in {
//      skip( "too stupid" )
//      import Gen._
//      val s = new Serializator( NTriple )
//      def uriRefs: Gen[UriRef] = for(s <- alphaStr) yield UriRef( s )
//      def blanks: Gen[Blank] = for(s <- alphaStr) yield Blank( s )
//      def lits: Gen[PlainLiteral] = for(s <- alphaStr) yield PlainLiteral( s )
//      implicit def arbTriple: Arbitrary[RdfTriple] =
//        Arbitrary(
//          for( s <- uriRefs|blanks; p <- uriRefs; o <- uriRefs|blanks|lits )
//            yield RdfTriple( s, p, o )
//        )
//      val prop = Prop.forAll( { triples: List[RdfTriple] =>
//        val ing = new jena.JenaGraph ++ triples
//        val w = new CharArrayWriter
//        s.write(ing, w)
//        val outg = new jena.JenaGraph ++ NTriplesParser( new CharArrayReader( w.toCharArray ) )
//        outg =~ ing
//      } )
//      prop.check
//    }
  }
}
