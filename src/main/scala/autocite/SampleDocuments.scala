package autocite

import autocite._
import com.twitter.util._

object SampleDocuments extends App {
  val master = Finagle.connect(
    conn => new Search.FinagledClient(conn),
    "localhost", 9999)

  case class DocAndMatches(doc: Document, matches: Seq[SearchResult])

  def documentAndMatches : DocAndMatches = {
      val doc = master.random.get.doc.get
      val cites = doc.outgoing.flatMap(
        cite => master.search(
        "\"%s\"".format(cite.title),
        "(s : ScoringInfo) => s.textScore").get.results.take(1))
      println("Finished ", doc.title, cites.length);
      DocAndMatches(doc, cites)
  }
  
  val candidates = 
    Range(0, 10)
    .map(_ => documentAndMatches)
    .filter(_.matches.length > 10)
  HadoopUtil.listToSequenceFile(candidates, "/tmp/results.seq")
}
