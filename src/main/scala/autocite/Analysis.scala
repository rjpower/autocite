package autocite

import java.nio.ByteBuffer
import java.security.MessageDigest
import scala.Array.canBuildFrom
import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }
import scala.util.parsing.input.CharSequenceReader
import scala.xml.Node
import com.twitter.logging.Logger
import autocite.XMLImplicits.nodeToHelper
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class Analysis(val xml: String) {
  lazy val dom = Analysis.parseXML(xml)
  lazy val pages = Analysis.pages(dom)
  lazy val textNodes = pages.flatten
  lazy val text = Analysis.extractText(textNodes)
  lazy val title = Analysis.extractTitle(textNodes)
  lazy val citations = Analysis.extractCitations(this)
  lazy val id = Analysis.titleToId(title)
}

object Analysis {
  private val log = Logger.get(getClass)

  type Page = Seq[TextNode]

  // A single <text...>...</text> node in the original document XML.
  case class TextNode(fonts: Map[Int, Int], val node: Node, val page: Int, val id: Int) {
    val left = node.iattr("left")
    val width = node.iattr("width")
    val right = left + width

    val top = node.iattr("top")
    val height = node.iattr("height")
    val bottom = top + height

    val center = left + width / 2
    val fontId = node.iattr("font")

    val bold = !(node \ "b").isEmpty
    val boldMultiplier = if (bold) { 2 } else { 1 }
    val fontSize = fonts.getOrElse(fontId, 0) * boldMultiplier

    // The text attributes strips any errant child nodes (such as <b> or <i>)
    val text = node.text
    override def toString = node.toString
  }

  val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
  val adapter = new scala.xml.parsing.NoBindingFactoryAdapter

  def parseXML(document: String): Node = {
    val dtd = "<!DOCTYPE pdf2xml SYSTEM \"pdf2xml.dtd\">"
    log.debug("Parsing document of size %d", document.length)
    adapter.loadXML(scala.xml.Source.fromString(document.split(dtd).last), parser)
  }

  def pages(xmlTree: Node): Seq[Page] = {
    try {
      val fonts = xmlTree.\\("fontspec")
        .map(node => (node.iattr("id"), node.iattr("size")))
        .toMap[Int, Int]

      var id = 0
      xmlTree.\("page").map(page => {
        page.\\("text").map(node => {
          id += 1
          TextNode(fonts, node, page.iattr("number"), id)
        })
      })
    } catch {
      case e: Exception => {
        log.info("Error while extracting text nodes from document. %s", e.toString)
        List()
      }
    }
  }

  def extractText(text: Seq[TextNode]): String = {
    if (text.isEmpty) {
      return ""
    }

    // We add a new line if drop appears in the original text, and the previous
    // line did not extend to the end of the column.:q
    val maxRight = text.map(n => n.left + n.width).max

    (for (i <- Range(0, text.length)) yield {
      val curNode = text(i)
      val prevNode = text(scala.math.max(i - 1, 0))
      val shift = scala.math.abs(curNode.left - (prevNode.left + prevNode.width))
      val drop = scala.math.abs(curNode.top - prevNode.top)
      val widthDiff = maxRight

      if (drop > 2 && widthDiff > 50 && !prevNode.text.endsWith("-")) {
        "\n" + curNode.text
      } else if (shift > 2 && !prevNode.text.endsWith("-")) {
        " " + curNode.text
      } else {
        curNode.text
      }
    }).map(_.stripSuffix("-")).toArray.mkString("")
  }

  def extractTitle(nodes: Seq[TextNode]): String = {
    if (nodes.isEmpty) { return "" }
    var lastNode: TextNode = null

    def scoreTitle(node: TextNode) = {
      if (node.text.isEmpty || !node.text(0).isUpper) {
        0.0
      } else {
        val hDistance = scala.math.abs(400 - node.center)
        val vDistance = scala.math.abs(100 - node.top)
        node.fontSize / (math.log(10 + vDistance) * math.log(10 + hDistance))
      }
    }

    def mergeTitle(titleNode: TextNode)(node: TextNode) = {
      if (lastNode == null) {
        lastNode = node
        true
      } else {
        scala.math.abs(node.height - titleNode.height) < 2 &&
          (node.top == lastNode.top || scala.math.abs(node.top - lastNode.top - lastNode.height) < 20)
      }
    }

    def goodTitle(title: String) = {
      title.length > 10 && title.length < 150
    }

    val pageone = nodes.filter(_.page == 1)
    val sorted = pageone.sortBy(scoreTitle).reverse.take(5).toList
    var title = ""

    for (titleNode <- sorted) {
      val top = pageone
        .dropWhile(_.id != titleNode.id)
        .takeWhile(mergeTitle(titleNode))
      title = top.map(_.text.trim).mkString(" ")
      if (goodTitle(title)) {
        return title
      }
    }

    ""
  }

  def titleToId(title: String): Long = {
    def stripPunctuation(v: String) = "[^\\w+]".r.replaceAllIn(v, "")
    def simplify(t: String) = stripPunctuation(t).toLowerCase()

    val md5 = MessageDigest.getInstance("MD5")
    ByteBuffer.wrap(md5.digest(simplify(title).getBytes)).getLong()
  }

  // A citation is a hairy beast.
  //
  // We start out assuming the common format of:
  //
  // (author)+ (title) (journal/conference)
  //
  // Unfortunately there is much variation between papers when it comes
  // to the delimiters within and between these sections.  This parser
  // relies on a number of heuristics to find a successful parse.

  class CitationParser extends RegexParsers with PackratParsers {
    val keyWords = Set("and", "or", "the", "if", "our")
    val conferenceWords = Set(
      "proc\\.", "inc\\.", "pp\\.",
      "proceedings", "conference",
      "acm", "ieee", "usenix")

    sealed trait N {
      def chars(): String
      def lower() = chars.toLowerCase
      def words() = chars.toLowerCase.split(" ")

      def isKeyWord() = containsWords(keyWords)
      def isConferenceWord() = containsWords(conferenceWords)
      def containsWords(s: Set[String]) = !(words.toSet & s).isEmpty
      def hasDate() = !words.flatMap("\\d\\d\\d\\d".r.findFirstIn).isEmpty
      def isNumeric() = "\\d".r.findFirstIn(chars).isDefined
    }

    case class Token(chars: String) extends N
    case class EOT(chars: String) extends N
    case class Separator(chars: String) extends N

    case class Title(chars: String) extends N
    case class Conference(chars: String) extends N

    case class Authors(vals: List[Author])

    def token(p: Parser[String]): Parser[Token] = p ^^ { w => Token(w.trim) }

    implicit def stringToTokenParser(p: Parser[String]): Parser[Token] = token(p)
    implicit def tokenToString(t: Token) = t.chars

    val phd: Parser[Token] = "ph\\.? ? d\\.?".r ^^^ { Token("PhD") }
    val etal: Parser[Token] = "(?i)et\\.? al\\.?".r ^^^ { Token("et al") }

    def peek[T](p: Parser[T]) = Parser {
      in =>
        p(in) match {
          case s @ Success(v, _) => Success(v, in)
          case f @ Failure(v, _) => Failure(v, in)
          case e @ Error(v, _)   => Error(v, in)
        }
    }

    class CappedBuffer extends ArrayBuffer[String] {
      override def +=(s: String) = {
        if (length < 500) {
          super.+=(s)
        }
        this
      }
    }

    val parserLog = new ArrayBuffer[String]

    def plog[T](p: Parser[T])(msg: String) = Parser { in =>
      parserLog += "Parsing " + msg
      parserLog += in.pos.longString
      var result = p(in)
      parserLog += result.toString
      result
    }

    lazy val special: Parser[Token] =
      conferenceWords
        .map(w => regex(("(?i)" + w).r))
        .reduce((a, b) => (a | b)) ^^ { Token(_) }

    lazy val endOfAuthors: Parser[EOT] = ("." | "," | "\"" | ";") ^^ { EOT(_) }

    lazy val initial: Parser[Token] = (
      "[\\p{Lu}][ ]".r ^^ { init => Token(init.trim) }
      | "[\\p{Lu}]".r <~ peek("[^\\p{L}]".r) ^^ { Token })

    lazy val initialDot: Parser[Token] = ("[\\p{Lu}]".r <~ "." ^^ { Token } | initial)
    lazy val nameLike: Parser[Token] =
      (etal
        | token("[\\p{Lu}][^0-9., ]+".r)
        | token("[\\p{Ll}]{2,4}".r))

    class Validator[T](val msg: String, f: Function[T, Boolean]) extends PartialFunction[T, T] {
      def isDefinedAt(t: T) = f(t)
      def apply(t: T) = t
    }

    def validator[T](msg: String, f: Function[T, Boolean])(implicit m: Manifest[T]) =
      new Validator[T](m.erasure.getName + "." + msg, f)

    def validate[T](p: Parser[T])(v: Seq[Validator[T]]) =
      v.foldLeft(p)((a, b) => memo(a ^? (b, t => b.msg + " :: " + t.toString)))

    val nameValidators = List(
      validator[Token]("TooLong", _.chars.length < 50),
      validator[Token]("IsKeyWord", !_.isKeyWord),
      validator[Token]("IsConferenceWord", !_.isConferenceWord),
      validator[Token]("HasNumbers", !_.isNumeric))
    lazy val name: Parser[Token] = validate(nameLike ^^ { t => Token(t.chars) })(nameValidators)

    // Different citation formats are:
    lazy val authorACM: Parser[Author] =
      (name ~ name ^^ { case a ~ b => Author(List(a, b)) }
        | name ~ initialDot ~ name ^^ { case a ~ b ~ c => Author(List(a, b, c)) }
        | initialDot ~ name ~ name ~ name ^^ { case a ~ b ~ c ~ d => Author(List(a, b, c, d)) }
        | initialDot ~ name ~ name ^^ { case a ~ b ~ c => Author(List(a, b, c)) }
        | initialDot ~ name ^^ { case a ~ b => Author(List(a, b)) }
        | initialDot ~ initialDot ~ name ^^ { case a ~ b ~ c => Author(List(a, b, c)) })

    lazy val authorIEEE: Parser[Author] = authorACM
    lazy val authorSpringer: Parser[Author] =
      (name ~ "," ~ initialDot ~ initialDot ^^ { case a ~ _ ~ b ~ c => Author(List(a, b, c)) }
        | name ~ "," ~ initialDot ^^ { case a ~ _ ~ b => Author(List(a, b)) })

    lazy val authorOpenAccess: PackratParser[Author] =
      name ~ "," ~ initial ^^ { case a ~ _ ~ b => Author(List(b, a)) }

    lazy val authorIOP: PackratParser[Author] =
      (name ~ initialDot ~ initial ^^ { case a ~ b ~ c => Author(List(a, b, c)) }
        | name ~ initial ^^ { case a ~ b => Author(List(a, b)) })

    lazy val defaultSeparator = ("," ~ "(?i)and".r) | "," | "(?)and".r

    def _authorList[A, B](author: Parser[Author], sep: Parser[A], eol: Parser[B]): PackratParser[Authors] =
      (author ~ sep ~ authorList(author, sep, eol) ^^ { case a ~ _ ~ b => Authors(a :: b.vals) }
        | author ~ opt(sep) ~ etal <~ opt(eol) ^^ { case a ~ _ ~ b => Authors(List(a, Author(List("et al")))) }
        | author <~ eol ^^ { case a => Authors(List(a)) })

    val authorValidators = List()
    def authorList[A, B](author: Parser[Author], sep: Parser[A], eol: Parser[B]): PackratParser[Authors] =
      _authorList(validate(plog(author)("author"))(authorValidators), sep, eol)

    lazy val authorsSpringer = authorList(authorSpringer, ",", ":")
    lazy val authorsIEEE = authorList(authorIEEE, defaultSeparator, regex("\\.|,".r))
    lazy val authorsACM = authorList(authorACM, defaultSeparator, ".")

    lazy val authorsIOP = authorList(authorIOP, ",", ".")
    lazy val authorsOpenAccess = authorList(authorIEEE, ";", " ")

    val authorListValidators = List(
      validator[Authors]("Empty", _.vals.length > 0),
      validator[Authors]("TooMany", _.vals.length < 8))

    lazy val authors: PackratParser[Authors] =
      validate(
        authorsSpringer |
          authorsACM |
          authorsIEEE |
          authorsIOP |
          authorsOpenAccess |
          authorsSpringer)(authorListValidators)

    //    val badTitles = Set("our", "its", "is", "the", "http://www")

    // Title parsing
    val titleValidators = List(
      validator[Title]("TooShort", _.chars.length > 5),
      validator[Title]("HasConference", !_.containsWords(conferenceWords)),
      validator[Title]("HasNumbers", !_.hasDate()))

    lazy val titleCaseWord: Parser[Token] = regex("[A-Z][^\\s.,]+".r)
    lazy val word: Parser[Token] = (special | token(regex("[^\\s.,]+".r)))
    lazy val endOfTitle: Parser[EOT] = ("." | "," | "\"" | ";") ^^ { EOT(_) }
    lazy val titleSeparator: Parser[Separator] = (":" | "-") ^^ { Separator(_) }

    lazy val titleLike: PackratParser[Title] =
      plog(word ~ titleLike ^^ { case Token(a) ~ Title(chars) => Title(a + " " + chars) }
        | endOfTitle ^^^ { Title("") })("titleLike")

    lazy val title: PackratParser[Title] = validate(
      "[\\p{Pi}]".r ~> "[^\\p{Pf}]+".r <~ "[\\p{Pf}]".r ^^ { Title }
        | titleCaseWord ~ titleLike ^^ { case Token(a) ~ Title(b) => Title(a + " " + b) })(titleValidators)

    // Conference parsing
    val conferenceValidators = List(
      validator[Conference]("TooShort", c => c.chars.length > 5),
      validator[Conference]("TooLong", c => c.chars.length < 200))

    lazy val inPrefix: Parser[String] = regex("(?i)In([: ])".r) <~ opt("the")

    lazy val confSeparator: Parser[Separator] = (":" | "-" | ",") ^^ { Separator(_) }

    // Stack overflow issues tend to occur when dealing with the conference
    // parsing via combinators, so do it by hand.
    val conferenceRest = new Parser[Conference] {
      def apply(in: Input): ParseResult[Conference] = {
        var conf = new StringBuffer()
        var rest = in

        while (conf.length < 500) {
          (initialDot | token("[0-9]+\\.?".r) | word)(rest) match {
            case Success(init, r) => {
              conf.append(init.chars)
              rest = r
            }
            case _ => {
              return Success(Conference(conf.toString.trim), rest)
            }
          }

          confSeparator(rest) match {
            case Success(v, r) => {
              conf.append(v.chars)
              rest = r
            }
            case _ => {}
          }

          conf.append(" ")
        }

        Failure("TooLong:: " + conf.toString, rest)
      }
    }
    plog(((initialDot | word) ~ confSeparator ~ conferenceRest
      | (initialDot | word) ~ conferenceRest
      | endOfTitle) ^^ {
        case EOT(sep)                                      => Conference("")
        case Conference(c)                                 => Conference(c)
        case Token(a) ~ Separator(sep) ~ Conference(chars) => Conference(a + sep + chars)
        case Token(a) ~ Conference(chars)                  => Conference(a + " " + chars)
      })("conf")

    lazy val conferenceStart: Parser[Token] =
      (special
        | titleCaseWord
        | token(regex("[0-9]+".r))
        | failure("StartOfConference"))

    lazy val conferenceLike: Parser[Conference] =
      (opt(inPrefix) ~>
        conferenceStart ~ confSeparator ~ conferenceRest ^^ { case Token(a) ~ Separator(b) ~ Conference(c) => Conference(a + b + c) }
        | conferenceStart ~ conferenceRest ^^ { case Token(a) ~ Conference(b) => Conference(a + " " + b) })

    lazy val conference: Parser[Conference] =
      validate(conferenceLike)(conferenceValidators)

    lazy val citation: Parser[Citation] =
      authors ~ title ~ conference ^^ {
        case Authors(authors) ~ Title(title) ~ Conference(conference) => {
          Citation(title.trim, authors, conference.trim)
        }
      }

    lazy val citeSeparator = "[^.\\n]+[.|\\n]".r

    val stripListIdentifiers = "\\[\\d+\\]".r

    def parseAll(str: String): Seq[Citation] = {
      parserLog.clear

      var cleanedStr = stripListIdentifiers.replaceAllIn(str, "")
      var results = new collection.mutable.LinkedList[Citation]

      var reader: Input = new PackratReader(new CharSequenceReader(cleanedStr))
      while (!reader.atEnd) {
        val cite = citation(reader)
        if (cite.successful) {
          //println("Successful!... %s\n%s\n%s".format( cite.get.title, cite.get.authors, cite.get.conference)) 
          reader = cite.next
          results +:= cite.get
        } else {
          parserLog += "------------------------- Failed Parse:"
          parserLog += cite.toString
          reader = citeSeparator(reader).next
          reader = reader.drop(1)
        }
      }

      results
    }
  }

  val theParser = new CitationParser()

  def extractCitations(a: Analysis): Seq[Citation] = {
    val txt = a.pages.takeRight(2).map(extractText)
    try {
      txt.flatMap(theParser.parseAll)
    } catch {
      case e: StackOverflowError => {
        log.error(theParser.parserLog.mkString("\n"))
        List()
      }
    }
  }
}
