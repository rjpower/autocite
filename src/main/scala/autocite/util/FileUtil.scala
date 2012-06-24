package autocite
import java.io.PrintWriter
import scala.io.Source
import java.io.FileInputStream
import java.io.InputStream
import java.io.File

object FileUtil {
  def dump(s: String, target: String) = {
    val w = new PrintWriter(target)
    w.print(s)
    w.close
  }
  
  def readAll(f : String) = {
    Source.fromFile(f).mkString("")
  }
}

object FileImplicits {
  class RichFile(f: File) {
    def read() : String = FileUtil.readAll(f.getAbsolutePath)
    def open() : InputStream = new FileInputStream(f)
    def dump(s : String) = FileUtil.dump(s, f.getAbsolutePath)
  }
  
  implicit def enrichFile(f : File) : RichFile = new RichFile(f)
}