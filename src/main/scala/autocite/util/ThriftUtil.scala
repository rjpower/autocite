package autocite.util

import java.io.{ StringReader, ByteArrayInputStream }
import java.net.{ InetSocketAddress, InetAddress }
import java.util.zip.{ GZIPOutputStream, GZIPInputStream }

import org.apache.commons.io.output._
import org.apache.thrift.protocol._
import org.apache.thrift.transport._

import com.twitter.conversions.time._
import com.twitter.finagle.builder._
import com.twitter.finagle.thrift._
import com.twitter.logging._
import com.twitter.ostrich.admin._
import com.twitter.scrooge._

object Thrift {
  def parseBinary[T <: ThriftStruct](
    codec: ThriftStructCodec[T],
    in: Array[Byte]) = {
    val prot = new TBinaryProtocol.Factory().getProtocol(
      new TMemoryInputTransport(in.toArray))
    codec.decoder(prot)
  }
}

object Finagle {
  def connect[T <: FinagleThriftClient](
    constructor: com.twitter.finagle.Service[ThriftClientRequest, Array[Byte]] => T,
    host: String, port: Int) = {
    val service = ClientBuilder()
      .hosts(new InetSocketAddress(host, port))
      .codec(ThriftClientFramedCodec())
      .hostConnectionLimit(16)
      .tcpConnectTimeout(3.seconds)
      .build()

    constructor(service)
  }
}


class FinagleApp[T <: Service](val rpcServer: T) extends autocite.AutociteApp {
  val runtime = RuntimeEnvironment(this, args)
  val adminFactory = AdminServiceFactory(
    httpPort = 9900,
    statsNodes = List(
      StatsFactory("Time Series",
        List(TimeSeriesCollectorFactory()))))
  val adminServer = adminFactory(runtime)

  try {
    rpcServer.start()
    Thread.sleep(10000000)
  } catch {
    case e: Exception => {
      e.printStackTrace()
      ServiceTracker.shutdown()
      System.exit(1)
    }
  }
}
