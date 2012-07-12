package autocite

import java.nio.ByteBuffer
import java.security.MessageDigest

import scala.xml.Node

import com.twitter.logging.Logger

import autocite.util.Implicits.enrichNode

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
      }).take(100)
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
    def stripPunctuation(v: String) = "[^a-zA-Z+]".r.replaceAllIn(v, "")
    def stripProceedings(v: String) = "(proc\\..*|proceedings.*)".r.replaceAllIn(v, "")
    def simplify(t: String) = stripPunctuation(stripProceedings(t)).toLowerCase()

    //println(simplify(title))
    val md5 = MessageDigest.getInstance("MD5")
    ByteBuffer.wrap(md5.digest(simplify(title).getBytes)).getLong()
  }

  val theParser = new CitationParser.MergedParser()

  def extractCitations(a: Analysis): Seq[Citation] = {
    val txt = a.pages.takeRight(2).map(extractText)
    txt.flatMap(theParser.parse)
  }
}
