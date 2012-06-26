package autocite

import java.net.InetAddress

import scala.Array.canBuildFrom
import scala.collection.mutable.HashMap

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.hadoop.io.{Text, SequenceFile}

import com.twitter.logging.Logger
import com.twitter.util.Future

import autocite.util.{Thrift, TextUtil, Finagle, FinagleApp}
import autocite.util.Thrift.Implicits._

abstract class Learner {
  val _rand = new java.util.Random
  val randStream = Stream continually { _rand.nextInt(10000000) }
  val log = Logger.get(getClass)
  var peers: Seq[Learning.FinagledClient] = null

  def fetch(title: String): Future[LookupResult] = {
    for (f <- Future.collect(peers.map(_.lookup(title)))) yield {
      f.seq.filter(_.doc.isDefined).lastOption.getOrElse(LookupResult(None))
    }
  }

  def apply(doc: Document)

  def run(d: Iterable[Document]) = {
    d.map(apply)
  }

  def finish(): AnyRef = { Unit }
}

class NaiveBayesLearner extends Learner {
  private val counts = List("good-cite", "bad-cite")
    .map(name => (name, new HashMap[String, Double]))
    .toMap

  private var totalCount = 0

  private def update(m: HashMap[String, Double], k: String) = {
    val v = m.get(k).getOrElse(0.0)
    m.put(k, v + 1.0)
    totalCount += 1
  }

  def train(label: String, text: String) {
    val cLabel = counts(label)
    TextUtil.ngram(text).map(ng => update(cLabel, ng))
  }

  def model(docid: Long): NaiveBayesModel = {
    // Normalize values and drop singleton counts prior to saving
    NaiveBayesModel(
      docid = docid,
      classes = counts.map({
        case (name, weights) =>
          (name,
            weights
            .filter({ case (key, value) => value > 1 })
            .map({ case (key, value) => (key, value.toDouble / totalCount) }))
      }).toMap)
  }

  def apply(doc: Document) = {
    val incomingDocs = doc.incoming
      .map(cite => fetch(cite.title))
      .map(_.get)
      .filter(_.doc.isDefined)
      .map(_.doc.get)

    val randomDocs = randStream
      .take(50)
      .map(idx => peers(idx % peers.length).random())
      .map(_.get)
      .filter(_.doc.isDefined)
      .map(_.doc.get)

    log.info("%s :: %d incoming, %d random",
      doc.title, incomingDocs.length, randomDocs.length)

    val docid = Analysis.titleToId(doc.title)

    incomingDocs.map(d => train("good-cite", new Analysis(d.xml).text))
    randomDocs.map(d => train("bad-cite", new Analysis(d.xml).text))
  }
}

class KMeansLearner(numClusters: Int) {
}

class LearnWorker extends Learning.ThriftServer {
  val serverName = "LearnWorker"
  val thriftPort = 19999

  val documents = new HashMap[String, Document]
  type Peer = Learning.FinagledClient

  // These values are initialized after documents have been loaded and 
  // prior to the start of the learning process
  var keys: Array[String] = null
  var peers: Seq[Peer] = null
  var learnerResults: SequenceFile.Writer = null

  val _rand = new java.util.Random

  def lookup(title: String) = {
    Future(LookupResult(documents.get(title)))
  }

  def random() = {
    if (keys.isEmpty) {
      Future(LookupResult(None))
    } else {
      val key = keys(_rand.nextInt(keys.length))
      Future(LookupResult(documents.get(key)))
    }
  }

  def load(shard: String) = {
    HadoopUtil.sequenceFileToStream(shard)
      .map({
        case (key, blob) =>
          val doc = Thrift.parseBinary(Document, blob)
          documents.put(doc.title, doc)
      }).length

    log.info("Done loading %s", shard)
    Future.Unit
  }

  def prepare(peerNames: Seq[String]) = {
    log.info("Got %d peers from master.", peerNames.length)
    peers = peerNames.map(host => Finagle.connect(s => new Learning.FinagledClient(s), host, 19999))
    keys = documents.keysIterator.toArray
    learnerResults = HadoopUtil.sequenceFileWriter[Text](
      "/autocite/learner-results/%s/%s".format(InetAddress.getLocalHost(), this.hashCode()))

    log.info("Setup finished.")
    Future.Unit
  }

  def learn() = {
    try {
      log.info("Starting Learning... (%d local documents)", keys.length)
      val l = new NaiveBayesLearner
      l.peers = peers
      l.run(documents.values)
    } catch {
      case e: Exception => {
        e.printStackTrace()
        log.error("Failed to run learner: %s", e)
      }
    }

    Future.Unit
  }
}

object LearnWorker extends FinagleApp(new LearnWorker())

object LearnMaster extends AutociteApp {
  val workerNames = Range.inclusive(14, 25).map(host => "beaker-%d".format(host))

  val workers = workerNames
    .map(host => Finagle.connect(s => new Learning.FinagledClient(s), host, 19999))

  def cleanup {
    val output = new Path("/autocite/learner-results.")
    val fs = output.getFileSystem(new Configuration)
    fs.delete(output, true)
  }

  def load {
    log.info("Loading...")
    val path = new Path("/autocite/doc-plus-cites")
    val fs = path.getFileSystem(new Configuration)
    val stats = fs.listStatus(path)
    val futures = stats
      .filter(_.getPath().getName().startsWith("part-"))
      .zipWithIndex
      .take(10)
      .map({
        case (file, idx) => workers(idx % workers.length).load(file.getPath.toString)
      })

    futures.map(_.apply)
  }

  def prepare {
    val names = workers.map(_.service)
    workers.map(_.prepare(workerNames)).map(_.apply)
  }

  def start {
    workers.map(_.learn).map(_.apply)
  }

  cleanup
  load
  prepare
  start
}
