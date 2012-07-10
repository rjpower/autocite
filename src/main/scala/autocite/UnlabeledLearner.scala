package autocite

import java.io.File
import autocite.util.Implicits._
import autocite.HadoopImplicits._
import org.apache.hadoop.io.BytesWritable
import org.apache.hadoop.io.Text
import autocite.util.Thrift
import autocite.util.Compress
import scala.collection.Map
import scala.collection.immutable.HashMap

object UnlabeledLearner {
  val splitter = "[0-9\\s\\p{Punct}]".r

  case class RichCite(cite: Citation) {
    lazy val id = Analysis.titleToId(cite.title)
  }

  implicit def enrichCite = RichCite
  implicit def makeOption[T](v: T) = Some(v)

  def docToBagOfWords(d: Document): Map[String, Double] = {
    var out = new HashMap[String, Double]
    for (w <- splitter.split(d.text.get.toLowerCase)) {
      out = out + Pair(w, 1.0 + out.getOrElse(w, 0.0))
    }
    out
  }

  def normalize(bow: Map[String, Double], idf: Map[String, Double], numDocs: Int) = {
    bow.map({
      case (k, v) => {
        (k, v * idf(k))
      }
    })
  }

  def computeIdf(bow: Map[Long, Map[String, Double]]): Map[String, Double] = {
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

  def computeCitations(sourceDir: String) = {
    new File(sourceDir)
      .listRecursive()
      .filter(_.getName().endsWith(".xml"))
      .map(f => {
        println(f.getName)
        val a = new Analysis(f.readAll)
        (a.id,
          Document(
            id = a.id,
            title = a.title,
            xml = "",
            incoming = null,
            outgoing = a.citations,
            text = a.text,
            url = f.getName))
      })
  }

  def buildCache(sourceDir: String, local: File, hadoop: String): LearnerCache = {
    if (local.exists) {
      val reader = HadoopUtil.sequenceFileReader[Text, BytesWritable](hadoop)
      val tIn = new Text
      val bIn = new BytesWritable
      reader.next(tIn, bIn)

      Thrift.parseBinary(LearnerCache, Compress.decompress(bIn))
    } else {
      println("Analyzing...")
      val target = HadoopUtil.sequenceFileWriter[Text, BytesWritable](hadoop)
      println("Cites...")
      val docs = computeCitations(sourceDir).toMap
      println("Words...")
      val bow = docs.mapValues(docToBagOfWords)
      println("IDF...")
      val idf = computeIdf(bow)
      println("Normalizing...")
      val normalized = bow.mapValues(b => normalize(b, idf, docs.size))
      println("Matching...")
      val matches = docs.mapValues(d => {
        d.outgoing.map(_.id).filter(docs.contains).toSet
      })
      println("Writing...")

      val cache = LearnerCache(docs, bow, normalized, idf, matches)
      target.append(new Text(""), new BytesWritable(cache.compressed))
      target.close
      cache
    }
  }

  def cosineSimilarity(a: Map[String, Double], b: Map[String, Double]): Double = {
    var total = 0.0
    val magnitude = b.values.sum
    for ((k, v) <- a) {
      total += v * b.getOrElse(k, 0.0)
    }
    total / magnitude
  }

  def findMatches(docs: Map[Long, Document]): Map[Long, Seq[Long]] = {
    println("Matching references...")
    docs.mapValues(
      _.outgoing.get
        .filter(cite => docs contains cite.id)
        .map(_.id))
  }

  def mostSimilar(a: Map[String, Double], docs: Map[Long, Map[String, Double]]) = {
    docs
      .toArray
      .map(b => (b._1, cosineSimilarity(a, b._2)))
      .sortBy(_._2)
  }
}

class UnlabeledLearner(sourceDir: String, cacheDir: String) {
  import UnlabeledLearner._
  val local = new File(cacheDir)
  val hadoop = "file:///" + local.getAbsolutePath()

  val learnerCache = buildCache(sourceDir, local, hadoop)

  val docs = learnerCache.docs
  val bow = learnerCache.bow
  val normalized = learnerCache.normalized
  val idf = learnerCache.idf
  val numDocs = docs.size
  val matches = learnerCache.matches

  def evaluate(docid: Long) {
    val doc = docs(docid)
    val realMatches = matches(docid).toSet
    val tfidfMatches = mostSimilar(bow(docid), normalized).takeRight(100).map(_._1).toSet

    val mutual = realMatches & tfidfMatches
    println("%-60.60s %3d/%3d (%3d) %.2f%%".format(
      doc.title, mutual.size, realMatches.size, doc.outgoing.toSet.size, 100.0 * mutual.size / realMatches.size))
  }
}

object UnlabeledAnalysis extends App {
  val learner = new UnlabeledLearner(args(0), args(1))

  val best = learner
    .matches
    .mapValues(_.size)
    .toArray
    .sortBy(_._2)
    .takeRight(50)
    .map(_._1)

  for (docid <- best) {
    learner.evaluate(docid)
  }

}
