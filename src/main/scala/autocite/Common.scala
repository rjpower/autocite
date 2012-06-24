package autocite
import scala.io.Source
import scala.io.Codec

object Common {
  def stripDtd(item: String) = {
    // Strip DTD lines from the document
    val dtdRe = "<!DOCTYPE pdf2xml[^>]+>".r
    dtdRe.replaceAllIn(item, "")
  }

  def stringForResource(r: String) = {
    val is = getClass.getResourceAsStream(r)
    new String(Source.fromInputStream(is, Codec.UTF8.name).toArray[Char])
  }

  lazy val xmlData = (for (i <- Range(1, 100)) yield {
    stringForResource("/paper-%05d.xml".format(i))
  }).toList.map(stripDtd)
  
}