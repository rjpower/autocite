package autocite.web

import java.net.InetSocketAddress

import org.mortbay.jetty.nio.SelectChannelConnector
import org.mortbay.jetty.webapp.WebAppContext
import org.mortbay.jetty.Server
import org.scalatra.scalate.ScalateSupport
import org.scalatra.ScalatraServlet

import com.twitter.conversions.time.intToTimeableNumber
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.thrift.ThriftClientFramedCodec
import com.twitter.logging.Logger

import autocite.util.Thrift.Implicits.thrift2json
import autocite.{Search, AutociteApp}

class AutociteServlet extends ScalatraServlet with ScalateSupport {
  val service = ClientBuilder()
    .hosts(new InetSocketAddress("localhost", 9999))
    .codec(ThriftClientFramedCodec())
    .hostConnectionLimit(1)
    .tcpConnectTimeout(3.seconds)
    .build()

  val log = Logger.get(classOf[AutociteServlet])

  val master = new Search.FinagledClient(service)

  log.info("Connected to %s", master)

  /**
   * Convert from a user query into Lucene syntax.
   *
   * Specifically, create an AND query from the users terms as well
   * as bigrams for each pair of terms in the original query.
   */
  def buildLuceneQuery(q: String) = {
    val terms = q.split(" ")
    val bigrams = for (i <- Range(0, terms.length - 1))
      yield "\"%s %s\"^3".format(terms(i), terms(i + 1))

    terms.mkString(" AND ") + " " + bigrams.mkString(" OR ")
  }

  get("/") {
    contentType = "text/html"
    layoutTemplate("search.jade")
  }

  get("/search") {
    val query = buildLuceneQuery({ params("q") })
    val scorer = { params("scorer") }
    val result = master.search(query, scorer).apply()
    log.info("Got result: %s", result.results.length)
    result.json
  }

  get("/lookup") {
    val shard = { params("shard") }
    val luceneDoc = { params("ldocid").toInt }
    master.lookup(shard, luceneDoc).get().json
  }

  notFound {
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map { path =>
      contentType = "text/html"
      layoutTemplate(path)
    } orElse serveStaticResource() getOrElse resourceNotFound()
  }
}

object SearchApp extends AutociteApp {

  //  System.setProperty("DEBUG", "true")
  //  System.setProperty("VERBOSE", "true")

  val server = new Server
  val scc = new SelectChannelConnector
  scc.setPort(8080)
  server.setConnectors(Array(scc))

  val context = new WebAppContext()
  context.setClassLoader(classOf[AutociteServlet].getClassLoader())
  context.setServer(server)
  context.setContextPath("/")
  context.setResourceBase("src/main/webapp")
  context.setLogUrlOnStart(true)
  server.addHandler(context)

  server.start()
}
