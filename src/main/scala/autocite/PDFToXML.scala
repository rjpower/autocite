package autocite

import java.io._
import autocite.util.FileImplicits._

import scala.collection.JavaConversions.asScalaIterator
import scala.sys.process._

import edu.cmu.lemurproject.WritableArchiveRecord

object PDFToXML {
  def apply(v: WritableArchiveRecord): Array[Byte] = {
    val blobWriter = new ByteArrayOutputStream()
    v.data.dump(blobWriter)
    blobWriter.close()

    val pdfBlob = blobWriter.toByteArray()
    val endOfHeader = blobWriter.toString().indexOf("\r\n\r\n")

    if (endOfHeader == -1 || pdfBlob.length < 5000) {
      return null
    }

    val url = v.data.getHeader().getUrl().toLowerCase()
    val binaryIn = new ByteArrayInputStream(pdfBlob, 4 + endOfHeader, pdfBlob.length - 4 - endOfHeader);
    apply(binaryIn, url)
  }
  
  def apply(filename : String) : Array[Byte] = apply(new File(filename).open(), filename)
 
  def apply(binaryIn : InputStream, name : String) : Array[Byte] = {
    val xmlOut = new ByteArrayOutputStream() 
    val processor = if (name.contains(".pdf")) { "pdftoxml" } else { "pstoxml" }
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
      return bytes
    } else {
      return null
    }
  }
}
