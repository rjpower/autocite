package autocite

import java.io.StringReader

import org.apache.lucene.util._
import org.apache.lucene.analysis._
import org.apache.lucene.analysis.shingle._
import org.apache.lucene.analysis.standard._
import org.apache.lucene.analysis.tokenattributes._

object TextUtil {
  def ngram(t: String) = {
    val filter = new ShingleFilter(
      new StandardTokenizer(
        Version.LUCENE_35, new StringReader(t)))

    val term = filter.getAttribute(classOf[CharTermAttribute])
    var result = List[String]()
    while (filter.incrementToken()) {
      result +:= term.toString()
    }
    result
  }
}

