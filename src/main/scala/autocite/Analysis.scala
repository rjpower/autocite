package autocite

import java.nio.ByteBuffer
import java.security.MessageDigest

import scala.Array.canBuildFrom
import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }
import scala.util.parsing.input.CharSequenceReader
import scala.xml.Node

import com.twitter.logging.Logger

import autocite.XMLImplicits.nodeToHelper

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

  object CitationParser extends RegexParsers with PackratParsers {
    val keyWords = Set("and", "or", "the", "if")
    val conferenceWords = Set(
      "proc\\.", "inc\\.", "pp\\.",
      "proceedings", "conference",
      "acm", "ieee", "usenix")

    sealed trait N {
      def chars(): String
      def lower() = chars.toLowerCase
      def words() = chars.toLowerCase.split(" ")

      def isKeyWord() = !(words.toSet & keyWords).isEmpty
      def isConferenceWord() = !(words.toSet & conferenceWords).isEmpty
      def hasDate() = !words.flatMap("\\d\\d\\d\\d".r.findFirstIn).isEmpty
      def isNumeric() = "\\d".r.findFirstIn(chars).isDefined
    }

    case class Token(chars: String) extends N
    case class EOT(chars: String) extends N
    case class Separator(chars: String) extends N

    case class Name(chars: String) extends N
    case class Title(chars: String) extends N
    case class Conference(chars: String) extends N

    case class Authors(vals: List[Author])

    implicit def node2string(t: N): String = t.chars

    // Strip out quotation marks.
    override val whiteSpace = "[\\s\"]+".r

    val phd: PackratParser[Token] = "ph\\.? ? d\\.?".r ^^^ { Token("PhD") }
    val etal: PackratParser[Token] = "(?i)et\\.? al\\.?".r ^^^ { Token("et al") }


    lazy val special: PackratParser[Token] =
      conferenceWords
        .map(w => regex(w.r))
        .reduce((a, b) => (a | b)) ^^ { Token(_) }

    lazy val word: PackratParser[Token] =
      (special | "[^\\s.,]+".r ^^ { Token(_) })

    lazy val endOfTitle: PackratParser[EOT] = ("." | "," | " ") ^^ { EOT(_) }
    lazy val endOfAuthors = endOfTitle

    lazy val separator: PackratParser[Separator] = (":" | "-") ^^ { Separator(_) }

    lazy val initial: PackratParser[Token] = "[A-Z]\\.?".r ^^ { Token(_) }
    lazy val nameLike: PackratParser[Name] = (
      etal ^^ { n => Name(n.chars) } |
      "[A-Z][^0-9., ]+".r ^^ { Name(_) } |
      "[a-z]{2,4}".r ^^ { Name(_) })

    class Validator[T](val msg: String, f: Function[T, Boolean]) extends PartialFunction[T, T] {
      def isDefinedAt(t: T) = f(t)
      def apply(t: T) = t
    }

    val nameValidators = List(
      new Validator[Name]("Name.TooShort", _.chars.length > 2),
      new Validator[Name]("Name.TooLong", _.chars.length < 50),
      new Validator[Name]("Name.IsKeyWord", !_.isKeyWord),
      new Validator[Name]("Name.IsConferenceWord", !_.isConferenceWord),
      new Validator[Name]("Name.HasNumbers", !_.isNumeric))

    val authorValidators = List(
      new Validator[Authors]("Authors.Empty", _.vals.length > 0),
      new Validator[Authors]("Authors.TooMany", _.vals.length < 8))

    val titleValidators = List(
      new Validator[Title]("Title.TooShort", _.chars.length > 5),
      new Validator[Title]("Title.HasAnd", !_.lower.startsWith("and")),
      new Validator[Title]("Title.HasConference", !_.isConferenceWord()),
      new Validator[Title]("Title.HasNumbers", !_.hasDate()))

    val conferenceValidators = List(
      new Validator[Conference]("Conference.TooShort", c => c.chars.length > 5))

    def validate[T](p: PackratParser[T], v: Seq[Validator[T]]) =
      v.foldLeft(p)((a, b) => memo(a ^? (b, t => b.msg + " :: " + t.toString)))

    lazy val name: PackratParser[Name] = validate(nameLike, nameValidators)

    // Different citation formats are:
    lazy val authorACM: PackratParser[Author] =
      (name ~ name ^^ { case a ~ b => Author(List(a, b)) }
        | name ~ initial ~ name ^^ { case a ~ b ~ c => Author(List(a, b, c)) }
        | initial ~ name ~ name ~ name ^^ { case a ~ b ~ c ~ d => Author(List(a, b, c, d)) }
        | initial ~ name ~ name ^^ { case a ~ b ~ c => Author(List(a, b, c)) }
        | initial ~ name ^^ { case a ~ b => Author(List(a, b)) }
        | initial ~ initial ~ name ^^ { case a ~ b ~ c => Author(List(a, b, c)) })

    lazy val authorIEEE: PackratParser[Author] = authorACM
    lazy val authorSpringer: PackratParser[Author] =
      (name ~ "," ~ initial ~ initial ^^ { case a ~ _ ~ b ~ c => Author(List(a, b, c)) }
        | name ~ "," ~ initial ^^ { case a ~ _ ~ b => Author(List(a, b)) })

    lazy val authorOpenAccess: PackratParser[Author] =
      name ~ "," ~ initial ^^ { case a ~ _ ~ b => Author(List(b, a)) }

    lazy val authorIOP: PackratParser[Author] =
      (name ~ initial ~ initial ^^ { case a ~ b ~ c => Author(List(a, b, c)) }
        | name ~ initial ^^ { case a ~ b => Author(List(a, b)) })

    lazy val defaultSeparator = "," ||| "and" ||| ("," ~ "and")

    def authorList[A, B](author: Parser[Author], sep: Parser[A], Node: Parser[B]): Parser[Authors] =
      (author ~ sep ~ authorList(author, sep, Node) ^^ { case a ~ _ ~ b => Authors(a :: b.vals) }
        | author ~ opt(sep) ~ etal <~ opt(Node) ^^ { case a ~ _ ~ b => Authors(List(a, Author(List("et al")))) }
        | author <~ Node ^^ { case a => Authors(List(a)) })

    lazy val authorsSpringer = authorList(authorSpringer, ",", ":")
    lazy val authorsIEEE = authorList(authorIEEE, defaultSeparator, regex("\\.|,".r))
    lazy val authorsACM = authorList(authorACM, defaultSeparator, ".")

    lazy val authorsIOP = authorList(authorIOP, ",", ".")
    lazy val authorsOpenAccess = authorList(authorIEEE, ";", " ")

    lazy val authors: PackratParser[Authors] = validate(
      authorsACM |||
        authorsSpringer |||
        authorsIEEE |||
        authorsIOP |||
        authorsOpenAccess, authorValidators)

    lazy val titleLike: PackratParser[Title] =
      (endOfTitle ||| word ~ titleLike) ^^ {
          case EOT(sep)                      => Title("")
          case Token(a) ~ Title(chars)       => Title(a + " " + chars)
        }

    lazy val title: PackratParser[Title] = validate(titleLike, titleValidators)

    lazy val conferenceLike: PackratParser[Conference] =
      (word ~ separator ~ conferenceLike
        | word ~ conferenceLike
        | endOfTitle) ^^ {
          case EOT(sep)                                      => Conference("")
          case Token(a) ~ Separator(sep) ~ Conference(chars) => Conference(a + sep + chars)
          case Token(a) ~ Conference(chars)                  => Conference(a + " " + chars)
        }

    lazy val conference: PackratParser[Conference] = validate(conferenceLike, conferenceValidators)

    lazy val citation: PackratParser[Citation] =
      authors ~ title ~ conference ^^ {
        case Authors(authors) ~ Title(title) ~ Conference(conference) => {
          Citation(title.trim, authors, conference.trim)
        }
      }

    lazy val citeSeparator = "[^.\\n]+[.|\\n]".r

    def parseAll(str: String): Seq[Citation] = {
      var results = new collection.mutable.LinkedList[Citation]

      var reader: Input = new PackratReader(new CharSequenceReader(str))
      while (!reader.atEnd) {
        val cite = citation(reader)
        if (cite.successful) {
          //println("Successful!... %s\n%s\n%s".format( cite.get.title, cite.get.authors, cite.get.conference)) 
          reader = cite.next
          results +:= cite.get
        } else {
          //println("Failure... ")
          //println(cite)
          reader = citeSeparator(reader).next
          reader = reader.drop(1)
          //println("seeking to:", reader.pos)
        }
      }

      results
    }
  }

  def extractCitations(a: Analysis): Seq[Citation] = {
    CitationParser.parseAll(extractText(a.pages.last))
  }
}
