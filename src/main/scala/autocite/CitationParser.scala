package autocite

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharSequenceReader
import com.twitter.logging.Logger

// A citation is a hairy beast.
//
// We start out assuming the common format of:
//
// (author)+ (title) (journal/conference)
//
// Unfortunately there is much variation between papers when it comes
// to the delimiters within and between these sections.  This parser
// relies on a number of heuristics to find a successful parse.

object CitationLexer extends Scanners with RegexParsers {
  override type Elem = Char

  def whitespace: Parser[Any] = "\\s+".r

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
    def isPunct() = "[\\p{P}]]".r.findFirstIn(chars).isDefined

    def hasDate() = !words.flatMap("\\d\\d\\d\\d".r.findFirstIn).isEmpty
  }

  def keyToken: Parser[Token] = keyTokens
    .map(w => literal(w) ^^ Token)
    .reduce((a, b) => a | b)

  def conferenceToken: Parser[Token] = conferenceTokens
    .map(w => literal(w) ^^ Token)
    .reduce((a, b) => a | b)

  val phd = "ph\\.? ? d\\.?".r ^^^ { Token("PhD") }
  val etal = "(?i)et\\.? al\\.?".r ^^^ { Token("et al") }
  val initial = "[\\p{Lu}].?".r ^^ Token
  val name = "[\\p{Lu}][\\p{Lu}]+]".r ^^ Token
  val comma = literal(",") ^^ Token
  val other = "[^\\s]+".r ^^ Token

  def token(): Parser[Token] = {
    keyToken |
      conferenceToken |
      phd |
      etal |
      initial |
      name |
      comma |
      other
  }

  def errorToken(msg: String) = Token("err.")
}

class CitationParser extends Parsers {
  type Elem = CitationLexer.Token
  type Token = CitationLexer.Token

  val log = Logger.get(classOf[CitationParser]);

  def token(s: String) = elem(s, _ => true)

  val name = elem("name", w => w.isUpper && !w.isInitial)
  val initial = elem("initial", _.isInitial)
  val comma = token(",")
  val word = elem("word", w => !w.isPunct())

  val author = name ~ rep1sep(initial, comma)
  val authors = rep1sep(author, comma) ^^ { _.map(Author) }
  val title = rep1(word) ^^ { _.mkString }

  val cite = authors ~ title ^^ {
    m => Citation(m._1, m._2, "")
  }

  def parseAll(str: String): Seq[Citation] = {
    val reader = new CitationLexer.Scanner(str)
    cite(reader) match {
      case Success(v, rest) => {}
      case Failure(v, rest) => {}
      case Error(v, rest) => {}
    }

    List()
  }
}
