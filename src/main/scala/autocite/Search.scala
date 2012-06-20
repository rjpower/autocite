package autocite

import java.io.File
import java.util.concurrent.Executors

import scala.Array.canBuildFrom
import scala.util.Random

import org.apache.commons.io.FileUtils
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.lucene.analysis.shingle.ShingleAnalyzerWrapper
import org.apache.lucene.index.IndexReader
import org.apache.lucene.queryParser.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version

import com.twitter.util.{FuturePool, Future, Eval}

case class ScoringInfo(val textScore: Double, val refs: Double, val title: String)

class SearchWorker extends Search.ThriftServer {
  val serverName = "SearchWorker"
  val thriftPort = 9999

  var indexShards: Map[String, IndexSearcher] = Map()
  var indexReaders: Map[String, IndexReader] = Map()
  val queryAnalyzer = new ShingleAnalyzerWrapper(Version.LUCENE_35, 2, 4)
  val queryParser = new QueryParser(Version.LUCENE_35, "text", queryAnalyzer)
  val evaluator = new Eval()

  val loadIndexPool = FuturePool(Executors.newFixedThreadPool(4))

  def loadIndex(srcDir: String) = {
    loadIndexPool.apply({
      val srcPath = new Path(srcDir)
      val indexPath = new Path("/scratch/autocite/index/%s".format(srcPath.getName()))

      val fs = srcPath.getFileSystem(new Configuration())
      val remoteFileInfo = fs.listStatus(srcPath)(0)

      val localDir = new File(indexPath.toString)
      if (!localDir.exists()
        || localDir.lastModified() < remoteFileInfo.getModificationTime()) {
        FileUtils.deleteDirectory(localDir)
        fs.copyToLocalFile(srcPath, indexPath)
      }

      val indexDir = FSDirectory.open(new File(localDir.getAbsolutePath))
      val reader = IndexReader.open(indexDir)
      indexReaders = indexReaders + Pair(srcDir, reader)
      indexShards = indexShards + Pair(srcDir, new IndexSearcher(reader))
      log.info("Loaded %s to local directory %s", srcDir, localDir.getAbsolutePath())
      true
    })
  }

  def searchLocalIndex(shard: String, text: String, scoringFunction: ScoringInfo => Double) = {
    // log.info("Searching index %s", shard)
    val idx = indexShards(shard)
    val query = queryParser.parse(text)

    val results = for (hit <- idx.search(query, 100).scoreDocs) yield {
      //      log.info("Result %s", res.doc)
      val lDoc = idx.doc(hit.doc)
      val title = lDoc.getFieldable("title").stringValue
      val refs = lDoc.getFieldable("references").stringValue.toInt
      val docid = lDoc.getFieldable("docid").stringValue.toLong
      SearchResult(title, scoringFunction(ScoringInfo(hit.score, refs, title)),
        refs, shard, docid, hit.doc)
    }

    results.toArray
  }

  type ScoringFunction = ScoringInfo => Double
  val scorerCache = scala.collection.mutable.Map[String, ScoringFunction]()

  def getScorer(scorerText: String): ScoringFunction = {
    scorerCache.getOrElseUpdate(scorerText,
      evaluator("import autocite._; " + scorerText, false).asInstanceOf[ScoringFunction])
  }

  def search(text: String, scorerText: String) = {
    val scoringFunction = getScorer(scorerText)	

    Future(SearchResults(
      indexShards.keys.flatMap(shard => {
        searchLocalIndex(shard, text, scoringFunction)
          .sortBy(_.score)
          .reverse
          .take(5)
      }).toList))
  }

  def lookup(shardId: String, docid: Long) = {
    val query = queryParser.parse("docid: %d".format(docid))
    if (indexShards.contains(shardId)) {
      val shard = indexShards(shardId)
      val hit = shard.search(query, 1).scoreDocs.head
      val docField = shard.doc(hit.doc).getFieldable("binary")
      val docBytes = docField.getBinaryValue.slice(docField.getBinaryOffset, docField.getBinaryLength)
      Future(LookupResult(
        Some(Thrift.parseBinary(Document, docBytes))))
    } else {
      Future(LookupResult(None))
    }
  }

  def random = {
    val shardList = indexShards.toList
    val shard = shardList(Random.nextInt(indexShards.size))._2
    val doc = shard.doc(Random.nextInt(shard.getIndexReader.numDocs))
    val docField = doc.getFieldable("binary")
    val docBytes = Compress.decompress(
      docField.getBinaryValue.slice(docField.getBinaryOffset, docField.getBinaryLength)
        .toArray)

    Future(LookupResult(
      Some(Thrift.parseBinary(Document, docBytes))))
  }
}

class SearchMaster extends Search.ThriftServer {
  val serverName = "SearchMaster"
  val thriftPort = 9999

  val workers = Range.inclusive(14, 25)
    .map(host => Finagle.connect(
      s => new Search.FinagledClient(s), "beaker-%d".format(host), thriftPort))

  def search(text: String, scorerText: String) = {
    log.info("Searching for %s", text)
    val collected = Future.collect(
      workers
        .map(_.search(text, scorerText)))

    collected.map(r =>
      SearchResults(
        r.seq
          .flatMap(_.results)
          .sortBy(_.score)
          .reverse))
  }

  def lookup(shard: String, docid: Long) = {
    val result = Future.collect(
      workers
        .map(_.lookup(shard, docid)))

    result.map(
      _.seq
        .filter(_.doc.isDefined)
        .last)
  }

  def random() = {
    workers(Random.nextInt(workers.length)).random
  }

  def loadIndex(dir: String) = {
    val path = new Path(dir)
    val fs = path.getFileSystem(new Configuration)
    val files = fs.listStatus(path)

    val loadFutures = files
      .filter(_.getPath.getName.startsWith("part-"))
      .zipWithIndex
      .map {
        case (path, index) => workers(index % workers.length).loadIndex(path.getPath.toString)
      }

    Future
      .join(loadFutures)
      .map(_ => true)
  }

  val loaded = loadIndex("/autocite/index").apply()
}

object SearchMaster extends FinagleApp(new SearchMaster())

object SearchWorker extends FinagleApp(new SearchWorker())
