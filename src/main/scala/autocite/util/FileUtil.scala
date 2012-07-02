package autocite.util

import java.io.PrintWriter
import scala.io.Source
import java.io.FileInputStream
import java.io.InputStream
import java.io.File
import java.io.FileReader
import java.io.BufferedReader

object FileUtil {
  def dump(s: String, target: String) = {
    val w = new PrintWriter(target)
    w.print(s)
    w.close
  }

  def readAll(f: File): String = {
    assert(f.exists, "Missing file.")
    // assert(f.length > 0, "Empty file.")
    
    val bytes = new Array[Byte](f.length.toInt)
    val in = new FileInputStream(f)
    in.read(bytes)
    in.close
    
    new String(bytes)
  }
}
