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

object UnlabeledLearner {

  class Helper(val doc: Document) {
    val splitter = "[0-9\\s\\p{Punct}]".r
    lazy val bagOfWords = {
      val m = new HashMap[String, Int]
      splitter
        .split(doc.text.toLowerCase)
        .filter(_.length > 1)
        .map(k => m.put(k, m.getOrElse(k, 0) + 1))
      m
    }

    lazy val id = Analysis.titleToId(doc.title)
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

  def idf(docs: Iterable[Helper]): Map[String, Int] = {
    var counts = new HashMap[String, Int]
    for (m <- docs) {
      for ((k, v) <- m.bagOfWords) {
        counts.put(k, counts.getOrElse(k, 0) + v)
      }
    }
    counts.toMap
  }

  def cosineSimilarity(a: Helper, b: Helper) = {
    val magnitude = a.bagOfWords.values.sum * b.bagOfWords.values.sum
    a.bagOfWords
      .map({ case (k, v) => b.bagOfWords.getOrElse(k, 0) * v })
      .reduce(_ + _) / magnitude.toDouble
  }

  def tfIdfSimiliary(a: Helper, b: Helper) = {

  }

  def findMatches(docs: Map[Long, Helper]) = {
    println("Matching references...")
    docs.map({
      case (k, v) => {
        (v, v.doc.outgoing.count(
          cite => docs contains Analysis.titleToId(cite.title)))
      }
    }).toList
  }

  def mostSimilar(a: Helper, docs: Iterable[Helper]) = {
    docs
      .map(b => (UnlabeledLearner.cosineSimilarity(a, b), b))
      .toArray
      .sortBy(_._1)
      .reverse
      .take(20)
  }
}

class UnlabeledLearner(sourceDir: String, cacheFile: String) {
  val local = new File(cacheFile)
  val hadoop = "file:///" + local.getAbsolutePath()

  def analyzeFiles {
    println("Analyzing...")
    if (!local.exists) {
      val target = HadoopUtil.sequenceFileWriter[Text, BytesWritable](hadoop)
      val allAnalysis = (new File(sourceDir)
        .listRecursive())
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

  val docs = UnlabeledLearner.loadData(hadoop).mapValues(doc => new UnlabeledLearner.Helper(doc))
  val matches = UnlabeledLearner.findMatches(docs)
  val counts = UnlabeledLearner.idf(docs.values)
}

object UnlabeledAnalysis extends App {
  val learner = new UnlabeledLearner(args(0), args(1))

  learner.counts
    .toArray
    .sortBy(_._2)
    .takeRight(20)
    .map(println)

  val best = learner.matches
    .toArray
    .sortBy(_._2)
    .takeRight(10)
    .map(_._1)

  for (a <- best) {
    println("Working....")

    val matches = UnlabeledLearner.mostSimilar(a, learner.docs.values)
    println(a.doc.title)
    for ((score, m) <- matches) {
      println("%4.5f %s".format(score, m.doc.title))
    }
  }

}
