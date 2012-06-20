package autocite

import java.net.{ InetSocketAddress, InetAddress }
import com.twitter.logging._

class AutociteApp extends App {
  val appName = getClass.getSimpleName()
  val hostName = InetAddress.getLocalHost().getHostName()

  private val logConfig = LoggerFactory(
    node = "",
    level = Some(Level.INFO),
    handlers = List(ConsoleHandler()))
  logConfig()

  val log = Logger.get(getClass)
}
