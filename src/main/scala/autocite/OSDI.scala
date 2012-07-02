package autocite

import java.io.File
import autocite.util.Implicits._
import autocite.HadoopImplicits._
import org.apache.hadoop.io.BytesWritable
import org.apache.hadoop.io.Text
import autocite.util.Thrift
import autocite.util.Compress
import scala.collection.mutable.MapBuilder
import scala.collection.mutable.HashMap

object OSDI {
  val whitespace = "\\s".r
  def docToBagOfWords(d: Document) = {
    val m = new HashMap[String, Int]
    whitespace
      .split(d.text)
      .map(k => m.put(k, m.getOrElse(k, 0) + 1))
    m
  }

  def idf(docs: Iterable[Document]): Map[String, Int] = {
    var counts = new HashMap[String, Int]
    for (m <- docs.map(docToBagOfWords)) {
      for ((k, v) <- m) {
        counts.put(k, counts.getOrElse(k, 0) + v)
      }
    }
    counts.toMap
  }

  def cosineSimilarity(a: Document, b: Document) = {
    val dA = docToBagOfWords(a)
    val dB = docToBagOfWords(b)
    val totalWords = dA.values.sum + dB.values.sum
    dA
      .map({ case (k, v) => dB.getOrElse(k, 0) * v })
      .reduce(_ + _) / totalWords.toDouble
  }

  def tfIdfSimiliary(a: Document, b: Document) = {

  }

  def findMatches(docs: Map[Long, Document]) = {
    println("Matching references")
    docs.map(
      {
        case (k, v) => {
          (v, v.outgoing.count(
            cite => docs contains Analysis.titleToId(cite.title)))
        }
      }).toList
  }
}

class OSDI(sourceDir: String, cacheFile: String) {
  val local = new File(cacheFile)
  val hadoop = "file:///" + local.getAbsolutePath()

  def analyzeFiles {
    println("Analyzing...")
    if (!local.exists) {
      val target = HadoopUtil.sequenceFileWriter[Text, BytesWritable](hadoop)
      val allAnalysis = new File(sourceDir)
        .listRecursive()
        .filter(_.getName().endsWith(".xml"))
        .map(f => {
          println(f.getName)
          val a = new Analysis(f.readAll)
          target.append(
            new Text(a.title),
            new BytesWritable(Document(a.title, a.xml, List(), a.citations, a.text, f.getName).compressed))
        })

      target.close
    }
  }

  def loadData(hadoopFile: String) = {
    println("Loading...")
    HadoopUtil
      .sequenceFileToStream[Text, BytesWritable](hadoopFile)
      .map({
        case (k, v) => {
          val doc = Thrift.parseBinary(Document, Compress.decompress(v))
          (Analysis.titleToId(doc.title), doc)
        }
      })
      .toList
      .toMap
  }

  val docs = loadData(hadoop)
  val matches = OSDI.findMatches(docs)
  val counts = OSDI.idf(docs.values)
}

object Analyze extends App {
  val o = new OSDI(args(0), args(1))

  val best = o.matches
    .toArray
    .sortBy(_._2)
    .takeRight(10)
    .map(_._1)

  for (doc <- best) {
    println("Working....")
    val matches = o.docs
      .values
      .map(b => (OSDI.cosineSimilarity(doc, b), b))
      .toArray
      .sortBy(_._1)
      .map(_._2)
      .take(20)

    println(doc.title)
    for (m <- matches) {
      println("  " + m.title)
    }
  }

}