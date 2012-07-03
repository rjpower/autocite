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

  val keyTokens = Set("and", "or", "the", "if", "our")
  val conferenceTokens = Set("proc\\.", "inc\\.", "pp\\.", "proceedings", "conference", "acm", "ieee", "usenix")

  case class Token(chars: String) {
    def lower() = chars.toLowerCase
    def words() = chars.toLowerCase.split(" ")

    private def containsTokens(s: Set[String]) = !(words.toSet & s).isEmpty

    def isUpper() = chars == chars.toUpperCase
    def isKeyToken() = containsTokens(keyTokens)
    def isConferenceToken() = containsTokens(conferenceTokens)
    def isNumeric() = "\\d".r.findFirstIn(chars).isDefined
    def isInitial() = isUpper && chars.length == 1

    def hasDate() = !words.flatMap("\\d\\d\\d\\d".r.findFirstIn).isEmpty

    override def toString = chars
  }

  implicit def tokenToString(t: Token) = t.chars

  class Parser extends Parsers with RegexParsers {
    type Token = CitationParser.Token

    def keyToken: Parser[Token] = keyTokens
      .map(w => literal(w) ^^ Token)
      .reduce((a, b) => a | b)

    def conferenceToken: Parser[Token] = conferenceTokens
      .map(w => literal(w) ^^ Token)
      .reduce((a, b) => a | b)

    val phd = "ph\\.? ? d\\.?".r ^^^ { Token("PhD") }
    val etal = "(?i)et\\.? al\\.?".r ^^^ { Token("et al") }
    val initial = "[\\p{Lu}][.]".r ^^ Token
    val nameUpper = "[\\p{Lu}][\\p{Lu}]+".r ^^ Token
    val nameRegular = "[\\p{Lu}][\\p{Ll}][^,.\\s]+".r ^^ Token
    val word = "[^.\\s]+".r ^^ Token
    val comma = literal(",") ^^ Token
    val punct = "[\\p{P}]+" ^^ Token
    val other = ".".r ^^ Token

    def token(): Parser[Token] =
      (keyToken |
        conferenceToken |
        phd |
        etal |
        initial |
        nameUpper |
        word |
        punct |
        other)

    def t(msg: String, f: Token => Boolean): Parser[Token] =
      token ^? { case tok if f(tok) => tok }

    val author: Parser[Author] =
      nameUpper ~ comma ~ rep1(initial) ^^ { case n ~ c ~ r => { Author(n :: r.map(_.chars)) } } |
      rep1(initial) ~ nameRegular ^^ { case r ~ n  => { Author(n :: r.map(_.chars)) } }

    val authors: Parser[Seq[Author]] = 
      author ~ "and" ~ author ~ "." ^^ { case a ~ _ ~ b ~ _ => List(a, b) } |
      rep1sep(author, comma) ~ "(?i), and".r ~ author ~ opt(".") ^^ { case l ~ a ~ r ~ _ => r :: l } |
      author ~ "." ^^ { case a ~ _ => List(a) }

    val title = rep1(word) ^^ { _.mkString(" ") }
    val cite = authors ~ title ^^ { case a ~ t => Citation(t, a, "") }

    def parseAll(str: String): Seq[Citation] = {
      val out = new ArrayBuffer[Citation]
      var reader: Input = new CharSequenceReader(str)
      while (!reader.atEnd) {
        val result = cite(reader)
        //println(result)

        result match {
          case Success(v, rest) => {
            out += v
            reader = rest
          }
          case Failure(v, rest) => {
            reader = token()(reader).next
          }
          case Error(v, rest) => {
            reader = token()(reader).next
          }
        }
      }
      out
    }
  }
}
