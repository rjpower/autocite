package autocite

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharSequenceReader
import com.twitter.logging.Logger
import scala.collection.mutable.ArrayBuffer

// A citation is a hairy beast.
//
// We start out assuming the common format of:
//
// (author)+ (title) (journal/conference)
//
// Unfortunately there is much variation between papers when it comes
// to the delimiters within and between these sections.  This parser
// relies on a number of heuristics to find a successful parse.

object CitationParser {
  val log = Logger.get()

  val keyWords = Set("and", "or", "the", "if", "our")
  val conferenceTokens = Set("proc\\.", "inc\\.", "pp\\.", "proceedings", "conference", "acm", "ieee", "usenix")

  case class Token(chars: String) {
    def lower() = chars.toLowerCase
    def words() = chars.toLowerCase.split(" ")

    private def containsTokens(s: Set[String]) = !(words.toSet & s).isEmpty

    def isUpper() = chars == chars.toUpperCase
    def isKeyToken() = containsTokens(keyWords)
    def isConferenceToken() = containsTokens(conferenceTokens)
    def isNumeric() = "\\d".r.findFirstIn(chars).isDefined
    def isInitial() = isUpper && chars.length == 1

    def hasDate() = !words.flatMap("\\d\\d\\d\\d".r.findFirstIn).isEmpty

    override def toString = chars
  }

  implicit def tokenToString(t: Token) = t.chars

  abstract class ParserBase extends Parsers with RegexParsers with PackratParsers {
    type Token = CitationParser.Token

    val keyWord: PackratParser[Token] =
      keyWords.map(w => literal(w) ^^ Token).reduce((a, b) => a | b)

    val conferenceToken: PackratParser[Token] =
      conferenceTokens.map(w => literal(w) ^^ Token).reduce((a, b) => a | b)

    var logLines = new ArrayBuffer[String]
    def plog[T](p: => Parser[T])(name: String): Parser[T] = Parser { in =>
      logLines.append("trying " + name + " at " + in)
      val r = p(in)
      logLines.append(name + " --> " + r)
      r
    }

    val phd = "ph\\.? ? d\\.?".r ^^^ { Token("PhD") }
    val etal = "(?i)et\\.? al\\.?".r ^^^ { Token("et al") }
    val initial = "[\\p{Lu}][.]".r ^^ Token
    val nameUpper = "[\\p{Lu}][\\p{Lu}]+".r ^^ Token
    val nameRegular = "[\\p{Lu}][\\p{Ll}][^,.\\s]+".r ^^ Token
    val comma = literal(",") ^^ Token
    val dquote = "[\\p{Pi}\\p{Pf}]".r ^^ Token
    val punct = "[\\p{P}]+".r ^^ Token
    val other = ".".r ^^ Token
    val word = "[^.?()\\s]+".r ^^ Token
    val date = "[0-9]+".r ~ punct

    val token: PackratParser[Token] = (conferenceToken | etal | initial | keyWord | nameUpper | other | phd | punct | word)

    // def t(msg: String, f: Token => Boolean): Parser[Token] = token ^? { case tok if f(tok) => tok }
    //

    def authorList(author: Parser[Author], sep: String, finalSep: String, terminator: String): PackratParser[List[Author]] =
      (author ~ sep ~ authorList(author, sep, finalSep, terminator) ^^ { case a ~ s ~ rest => a :: rest } |
        author ~ finalSep ~ author <~ terminator ^^ { case a ~ s ~ b => List(a, b) } |
        author <~ terminator ^^ { case a => List(a) })

    val cite: (Input => ParseResult[Citation]) = null

    def parse(str: String): Seq[Citation] = {
      logLines.clear
      val out = new ArrayBuffer[Citation]
      var reader: Input = new PackratReader(new CharSequenceReader(str))
      while (!reader.atEnd) {
        val result = cite(reader)
        //println(result)

        result match {
          case Success(v, rest) => {
            out += v
            reader = rest
          }
          case Failure(v, rest) => {
            reader = token(reader).next
          }
          case Error(v, rest) => {
            reader = token(reader).next
          }
        }
      }
      out
    }
  }

  class ACMParser extends ParserBase {
    val author: PackratParser[Author] =
      nameUpper ~ comma ~ rep1(initial) ^^ { case n ~ c ~ r => { Author(n.chars :: r.map(_.chars)) } } |
        rep1(initial) ~ nameRegular ^^ { case r ~ n => { Author(n.chars :: r.map(_.chars)) } }

    val authors: PackratParser[Seq[Author]] =
      author ~ "and" ~ author ~ "." ^^ { case a ~ _ ~ b ~ _ => List(a, b) } |
        rep1sep(author, comma) ~ "(?i), and".r ~ author ~ opt(".") ^^ { case l ~ a ~ r ~ _ => r :: l } |
        author ~ "." ^^ { case a ~ _ => List(a) }

    val title = opt(date) ~> rep1(word) ^^ { _.mkString(" ") }
    override val cite = authors ~ title ^^ { case a ~ t => Citation(t, a, "") }
  }

  class IEEEParser extends ParserBase {
    val title = dquote ~> "[^\\p{Pi}\\p{Pf}]+".r <~ dquote
    val author: PackratParser[Author] = rep1(initial) ~ nameRegular ^^ {
      case init ~ lastName => { Author(init.map(_.chars) :+ lastName.chars) }
    }

    val authors = authorList(author, ",", "and", ",")
    override val cite = authors ~ title ^^ { case a ~ t => Citation(t, a, "") }
  }

  class SpringerParser extends ParserBase {
    val title = "[^.]+".r
    val author: PackratParser[Author] =
      nameRegular ~ "," ~ rep1(initial) ^^ { case n ~ comma ~ init => Author(n.chars :: init.map(_.chars)) } |
        etal ^^^ Author(List("et al"))

    val authors = authorList(author, ",", ",", ":")
    override val cite = authors ~ title ^^ { case a ~ t => Citation(t, a, "") }
  }

  class MergedParser {
    val parserList = List(new ACMParser) // List(new ACMParser, new IEEEParser, new SpringerParser)
    import scala.math._

    def logLines = parserList.flatMap(_.logLines)
    def authorsScore(a: Seq[Author]) = a.size
    def titleScore(t: String) = t.size
    def confScore(c: String) = 1

    def scoreResults(cites: Seq[Citation]): Double = {
      val lengthScore = 1.0 / (1 + abs(cites.size - 25))
      val citeScore = cites
        .map(c => (authorsScore(c.authors) * titleScore(c.title) * confScore(c.conference)))
        .sum

      citeScore * lengthScore
    }

    def parse(str: String): Seq[Citation] = {
      parserList
        .map(_.parse(str))
        .map(citeList => (scoreResults(citeList), citeList))
        .sortBy(_._1)
        .last._2
    }
  }
}
