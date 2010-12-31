package org.scardf

import org.specs._
import org.specs.runner.JUnit4
import java.io.{CharArrayReader, CharArrayWriter}

class SerializerSpecsTest extends JUnit4(SerializerSpecs)

object SerializerSpecs extends Specification {
  val g = Doe.graph
  "Graph serialization / deserialization" should {
    "be round-tripable" in {
      val s = new Serializator( NTriple )
      val w = new CharArrayWriter
      s.write(g, w)
      val r = new CharArrayReader( w.toCharArray )
      s.readFrom(r) mustVerify( _ =~ g )
    }
  }
}
