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
  class Helper(val doc: Document) {
    val splitter = "[0-9\\s\\p{Punct}]".r
    
    lazy val bagOfWords = {
      var m = new HashMap[String, Double]
      splitter
        .split(doc.text.toLowerCase)
        .filter(_.length > 1)
        .map(k => {
            m = m + Pair(k, m.getOrElse(k, 0.0) + 1.0)
        })
      m
    }

    lazy val id = Analysis.titleToId(doc.title)
    var normalized : Map[String, Double] = null

    def normalize(counts : Map[String, Double]) = {
      if (normalized == null) {
        normalized = bagOfWords.map({ case (k, v) => (k, v.toDouble / counts(k)) })
      }

      normalized
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

  def idf(docs: Iterable[Helper]): Map[String, Double] = {
    var counts = new HashMap[String, Double]
    for (m <- docs.toArray) {
      for ((k, v) <- m.bagOfWords) {
        counts = counts + Pair(k, counts.getOrElse(k, 0.0) + v)
      }
    }
    counts
  }

  def cosineSimilarity(a: Map[String, Double], b: Map[String, Double]) : Double = {
    var total = 0.0
    val magnitude = b.values.sum
    for ((k, v) <- a) {
      total += v * b.getOrElse(k, 0.0)
    }
    total / magnitude
  }
  
  def cosineSimilarity(a: Helper, b: Helper) : Double = cosineSimilarity(a.bagOfWords, b.bagOfWords)

  def tfIdfSimiliarity(counts: Map[String, Double])(a: Helper, b: Helper) = {
    cosineSimilarity(a.normalize(counts), b.normalize(counts))
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

  def mostSimilar(a: Helper, docs: Iterable[Helper], metric: (Helper, Helper) => Double) = {
    docs
      .toArray
      .map(b => (metric(a, b), b))
      .sortBy(_._1)
      .takeRight(20)
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

//  def save() {
//    val oo = new ObjectOutputStream(new GZipOutputStream(new FileOutputStream("data.out")))
//    oo.writeObject(docs)
//    oo.writeObject(matches)
//    oo.writeObject(counts)
//    oo.Close()
//  }


  analyzeFiles
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


  def printMatches(title : String, matches : Seq[(Double, UnlabeledLearner.Helper)]) {
    println(title)
    for ((score, m) <- matches) {
      println("%4.5f %s".format(score, m.doc.title))
    }
  }
  
  val best = learner.matches
    .toArray
    .sortBy(_._2)
    .takeRight(20)
    .map(_._1)

  for (a <- best) {
    println("Working....")

    // printMatches(a.doc.title, UnlabeledLearner.mostSimilar(a, learner.docs.values, UnlabeledLearner.cosineSimilarity))
    printMatches(a.doc.title, UnlabeledLearner.mostSimilar(a, learner.docs.values, UnlabeledLearner.tfIdfSimiliarity(learner.counts)))
  }

}
