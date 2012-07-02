package autocite.util
import org.apache.thrift.protocol.TBinaryProtocol
import java.io.FileInputStream
import org.apache.thrift.protocol.TSimpleJSONProtocol
import org.apache.thrift.transport.TMemoryBuffer
import com.twitter.scrooge.ThriftStruct
import java.io.InputStream
import java.io.File
import org.apache.lucene.analysis.shingle.ShingleFilter
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.standard.StandardTokenizer
import java.io.StringReader
import java.io.ByteArrayInputStream
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream
import org.apache.commons.io.output.ByteArrayOutputStream


object Compress {
  def compress(in: Array[Byte]): Array[Byte] = {
    val bOut = new ByteArrayOutputStream()
    val zOut = new GZIPOutputStream(bOut)
    zOut.write(in)
    zOut.close
    bOut.toByteArray
  }

  def decompress(in: Array[Byte]): Array[Byte] = {
    val bIn = new ByteArrayInputStream(in)
    val zIn = new GZIPInputStream(bIn)
    val bOut = new ByteArrayOutputStream()
    bOut.write(zIn)
    bOut.toByteArray
  }
}

object Implicits {
  class RichThriftStruct(in: ThriftStruct) {
    def json(): String = {
      val buffer = new TMemoryBuffer(100)
      val protocol = new TSimpleJSONProtocol.Factory().getProtocol(buffer)
      in.write(protocol)
      val a = buffer.getArray()
      new String(buffer.getArray, 0, buffer.length())
    }

    def binary(): Array[Byte] = {
      val buffer = new TMemoryBuffer(100)
      val protocol = new TBinaryProtocol.Factory().getProtocol(buffer)
      in.write(protocol)
      val a = buffer.getArray()
      a.view(0, a.size).toArray
    }
    
    def compressed() : Array[Byte] = {
      Compress.compress(binary)
    }
  }
  
  object RichFile {
    def listRecursive(f : File) : Seq[File] = {
      if (!f.isDirectory()) {
        List(f)
      } else {
        f.listFiles().flatMap(listRecursive)
      }
    }
  }

  class RichFile(f: File) {
    def readAll(): String = FileUtil.readAll(f)
    def open(): InputStream = new FileInputStream(f)
    def dump(s: String) = FileUtil.dump(s, f.getAbsolutePath)
    def listRecursive() = RichFile.listRecursive(f)
  }

  class NGramString(t: String) {
    def ngram() = {
      val filter = new ShingleFilter(
        new StandardTokenizer(
          org.apache.lucene.util.Version.LUCENE_35,
          new StringReader(t)))

      val term = filter.getAttribute(classOf[CharTermAttribute])
      var result = List[String]()
      while (filter.incrementToken()) {
        result +:= term.toString()
      }
      result
    }
  }

  /**
   * Helper class for dealing with XML attributes.
   *
   * Adds the attr(name : String) method to a node, which
   * returns the text value of the first matching attribute.
   */
  final class NodeView(val n: scala.xml.Node) {
    def attr(name: String): String = {
      n.attribute(name).get(0).text
    }

    def iattr(name: String): Int = {
      n.attribute(name).get(0).text.toInt
    }
  }

  implicit def enrichNode(n: scala.xml.Node) = new NodeView(n)
  implicit def enrichThrift(in: ThriftStruct) = new RichThriftStruct(in)
  implicit def enrichFile(f: File) = new RichFile(f)
  implicit def enrichString(s: String) = new NGramString(s)
}
