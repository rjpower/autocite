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
import org.apache.commons.collections.map.LRUMap

object UnlabeledLearner {
  val splitter = "[0-9\\s\\p{Punct}]".r

  case class RichCite(cite: Citation) {
    lazy val id = Analysis.titleToId(cite.title)
  }

  implicit def enrichCite = RichCite
  implicit def makeOption[T](v: T) = Some(v)

  type BagOfWords = Map[String, Double]
  type Metric = (Long, Long) => Double

  def docToBagOfWords(d: Document): BagOfWords = {
    var out = new HashMap[String, Double]
    for (w <- splitter.split(d.text.get.toLowerCase)) {
      out = out + Pair(w, 1.0 + out.getOrElse(w, 0.0))
    }
    out
  }

  def normalize(bow: BagOfWords, idf: BagOfWords, numDocs: Int) = {
    bow.map({ case (k, v) => { (k, v * idf(k)) } })
  }

  def computeIdf(bow: Map[Long, BagOfWords]): BagOfWords = {
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
        (a.id, Document(id = a.id,
          title = a.title,
          xml = "",
          incoming = List(),
          outgoing = a.citations,
          text = a.text,
          url = f.getName,
          year = a.year))
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

  def cosineSimilarity(a: BagOfWords, b: BagOfWords): Double = {
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

  val cache = new collection.mutable.HashMap[Long, Array[(Long, Double)]]

  def cosineMetric(a: Long, b: Long) = cosineSimilarity(bow(a), bow(b))
  def tfidfMetric(a: Long, b: Long) = cosineSimilarity(normalized(a), normalized(b))

  def mostSimilar(a: Long, m: Metric) = {
    docs
      .map(b => (b._1, m(a, b._1)))
      .toArray
      .sortBy(_._2)
      .reverse
      .take(1000)
  }

  def evaluate(docid: Long, N: Int = 100, metric: Metric) = {
    val doc = docs(docid)
    val realMatches = matches(docid).toSet.take(N)
    val topMatches = cache.getOrElseUpdate(docid, mostSimilar(docid, metric))
    val tfidfMatches = topMatches.take(N).map(_._1).toSet

    val mutual = realMatches & tfidfMatches
    (mutual.size, realMatches.size)
  }

  def precisionRecall(docs: Seq[Long], metric: Metric) {
    cache.clear
    for (N <- Range(10, 100)) {
      val (a, b) = docs.map(evaluate(_, N, metric)).unzip
      val totalMatched = a.sum
      val totalFound = b.sum
      println("%s %s %s %s".format("tfidf", N, totalMatched, totalFound))
    }
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

  learner.precisionRecall(best, learner.cosineMetric)
  learner.precisionRecall(best, learner.tfidfMetric)

  for (docid <- best) {
    val doc = learner.docs(docid)
    val total = doc.outgoing.size
    val (matched, found) = learner.evaluate(docid, 100, learner.tfidfMetric)
    println("%-60.60s %3d/%3d (%3d) %.2f%%".format(
      doc.title, matched, found, total, 100.0 * matched / found))
  }

}
