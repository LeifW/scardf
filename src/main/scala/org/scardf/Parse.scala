package org.scardf

import java.io.Reader

/**
 * Attempting to follow the EBNF at http://www.w3.org/TR/rdf-testcases/#ntriples
 * Not fully parsing language tags yet.
 */

object Parse {
  val space = 32
  val tab = 9
  val sb = new StringBuilder(256)

  def apply(reader: Reader) = {
    val graph = new MutableSetGraph()
    var line = 1
    var char = reader.read()

    def optionalWhitespace {
      while ((char == space || char == tab) && char != -1) char = reader.read
    }
    def whitespace {
      if (char != space && char != tab)
        error("Whitespace expected on line " + line + ", '" + char.toChar + "' found.")
      else
        optionalWhitespace
    }

    def eoln {
      if (char == 13) {
        char = reader.read
        if (char == 10)
          char = reader.read
      } else if (char == 10) {
        char = reader.read
      } else {
        error("Line-end expected on line " + line + ", '" + char.toChar + "' found.")
      }
    }

    def takeUntil(c: Int) = {
      sb.clear()
      while (char != c && char != -1) {
        sb.append(char.toChar)
        char = reader.read
      }
      char = reader.read
      sb.toString
    }

    def lowercaseChars = {
      sb.clear()
      while (Character.isLowerCase(char) && char != -1) {
        sb.append(char.toChar)
        char = reader.read
      }
      sb.toString
    }

    def alphaNumericChars = {
      sb.clear()
      while (Character.isLetterOrDigit(char) && char != -1) {
        sb.append(char.toChar)
        char = reader.read
      }
      sb.toString
    }

    def require(c: Char) {
      if (char != c)
        error("'" + c + "' expected on line " + line + ", '" + char.toChar + "' found.")
      else
        char = reader.read
    }

    def uriref = UriRef({
      require('<')
      takeUntil('>')
    })

    def blank = Blank({
      require('_')
      require(':')
      alphaNumericChars
    })

    def subjectNode = char match {
      case '<' => uriref
      case '_' => blank
      case other => error("'<' or '_' expected on line " + line + ", '" + other.toChar + "' found.")
    }

    def literal = {
      require('"')
      val string = takeUntil('"')
      char match {
        case '^' => {
          char = reader.read
          require('^')
          TypedLiteral(string, uriref)
        }
        case '@' => {
          char = reader.read
          PlainLiteral(string, Some(LangTag(lowercaseChars))) // TODO: Handle the possibility of ('-' [a-z0-9]+ )*
        }
        case other => PlainLiteral(string)
      }
    }

    def node = char match {
      case '"' => literal
      case '<' => uriref
      case '_' => blank
      case other => error("'\"' or '<' expected on line " + line + ", '" + other.toChar + "' found.")
    }

    // The actual parsing loop
    while (char != -1) {

      optionalWhitespace
      val subject = subjectNode
      whitespace
      val predicate = uriref
      whitespace
      val obj = node
      optionalWhitespace
      require('.')
      optionalWhitespace
      eoln

      line = line + 1

      graph + RdfTriple(subject, predicate, obj)
    }

    reader.close()
    graph
  }
}
