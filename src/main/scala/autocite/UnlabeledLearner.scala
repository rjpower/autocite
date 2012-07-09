package autocite

import java.io.File
import autocite.util.Implicits._
import autocite.HadoopImplicits._
import org.apache.hadoop.io.BytesWritable
import org.apache.hadoop.io.Text
import autocite.util.Thrift
import autocite.util.Compress
import scala.collection.immutable.HashMap

object UnlabeledLearner {
  type BagOfWords = Map[String, Double]

  val splitter = "[0-9\\s\\p{Punct}]".r

  class RichCite(cite : Citation) {
    lazy val id = Analysis.titleToId(cite.title)
  }

  implicit def enrichCite = RichCite

  def docToBagOfWords(d: Document): BagOfWords = {
    var out = new HashMap[String, Double]
    for (w <- splitter.split(d.text.toLowerCase)) {
      out = out + Pair(w, 1.0 + out.getOrElse(w, 0.0))
    }
    out
  }

  def normalize(bow: BagOfWords, idf: BagOfWords, numDocs: Int) = {
    bow.map({
      case (k, v) => {
        (k, v * idf(k))
      }
    })
  }

  def idf(bow: Map[Long, BagOfWords]): BagOfWords = {
    var idf = new HashMap[String, Double]
    for (m <- bow.values) {
      for ((k, v) <- m) {
        idf = idf + Pair(k, idf.getOrElse(k, 0.0) + 1)
      }
    }

    println(idf.take(10).mkString("\n"))
    for ((k, v) <- idf) {
      idf = idf.updated(k, scala.math.log(bow.size / idf(k)))
    }
    idf
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
      .toMap
  }

  def cosineSimilarity(a: BagOfWords, b: BagOfWords): Double = {
    var total = 0.0
    val magnitude = b.values.sum
    for ((k, v) <- a) {
      total += v * b.getOrElse(k, 0.0)
    }
    total / magnitude
  }

  def findMatches(docs: Map[Long, Document]) : Map[Long, Seq[Long]] = {
    println("Matching references...")
    docs.mapValues(
        _.outgoing
          .filter(cite => docs contains cite.id)
          .map(_.id))
          .toArray
  }

  def mostSimilar(a: BagOfWords, docs: Map[Long, BagOfWords])= {
    docs
      .toArray
      .map(b => (b._1, cosineSimilarity(a, b._2)))
      .sortBy(_._2)
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

  analyzeFiles
  val docs = UnlabeledLearner.loadData(hadoop)
  val numDocs = docs.size
  println("Computing bag of words")
  val bow = docs.mapValues(UnlabeledLearner.docToBagOfWords)
  println("Computing idf")
  val idf = UnlabeledLearner.idf(bow)
  println("Matching")
  val matches = UnlabeledLearner.findMatches(docs)
  println("Normalizing")
  val normalized = bow.mapValues(v => UnlabeledLearner.normalize(v, idf, numDocs))
}

object UnlabeledAnalysis extends App {
  val learner = new UnlabeledLearner(args(0), args(1))

  def printMatches(title: String, matches: Seq[(Long, Double)]) {
    println(title)
    for ((docid, score) <- matches.reverse) {
      val doc = learner.docs(docid)
      println("%4.5f %s".format(score, doc.title))
    }
  }

  val best = learner.matches
    .sortBy(_._2)
    .takeRight(20)
    .map(_._1)

  for (doc <- best) {
    println("Processing....", doc.title)

    printMatches(doc,
      UnlabeledLearner.mostSimilar(
        learner.bow(Analysis.titleToId(doc.title)),
        learner.normalized))
  }

}
