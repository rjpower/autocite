package autocite

import java.io._
import scala.collection.JavaConversions.asScalaIterator
import scala.sys.process._
import org.apache.commons.logging.LogFactory
import org.apache.hadoop.io.{Text, LongWritable}
import org.apache.hadoop.mapred._
import edu.cmu.lemurproject.WritableArchiveRecord

object PDFToXML {
  def apply(v: WritableArchiveRecord): Option[Array[Byte]] = {
    val blobWriter = new ByteArrayOutputStream()
    v.data.dump(blobWriter)
    blobWriter.close()

    val pdfBlob = blobWriter.toByteArray()
    val endOfHeader = blobWriter.toString().indexOf("\r\n\r\n")

    if (endOfHeader == -1 || pdfBlob.length < 5000) {
      return None
    }

    val url = v.data.getHeader().getUrl().toLowerCase()
    val processor = if (url.contains(".pdf")) { "pdftoxml" } else { "pstoxml" }
    val binaryIn = new ByteArrayInputStream(pdfBlob, 4 + endOfHeader, pdfBlob.length - 4 - endOfHeader);
    apply(binaryIn, processor)
  }
  
  def apply(binaryIn : InputStream, processor : String) : Option[Array[Byte]] = {
    val xmlOut = new ByteArrayOutputStream()

    val pbThread = new Thread(
      new Runnable {
        def run() {
          Process(processor) #< binaryIn #> xmlOut !
        }
      })
    
    pbThread.start()

    // It might take a while to process...
    pbThread.join(10000)
    pbThread.interrupt()

    val bytes = xmlOut.toByteArray
    if (bytes.length > 100) {
      return Some(bytes)
    } else {
      return None
    }
  }
}
