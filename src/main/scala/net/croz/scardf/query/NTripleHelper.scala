package net.croz.scardf.query

import com.hp.hpl.jena.rdf.model.Resource
import java.io.StringWriter
import java.io.PrintWriter
import com.hp.hpl.jena.rdf.model.impl.NTripleWriter

object NTripleHelper {
  def ntRendering(  r:Resource )= {
    val sw = new StringWriter()
    val pw = new PrintWriter( sw )
    NTripleWriter.writeResource( r, pw )
    sw.toString();
  }
}