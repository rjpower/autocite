package autocite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import com.twitter.logging.{ LoggerFactory, Level, ConsoleHandler }
import com.twitter.logging.Logger
import scala.io.Source
import java.io._
import autocite.FileImplicits._

@RunWith(classOf[JUnitRunner])
class ExtractionSpec extends FunSuite with ShouldMatchers {
  LoggerFactory(
    node = "",
    level = Some(Level.INFO),
    handlers = List(ConsoleHandler()))()
  private val log = Logger.get(getClass)

  def printCites(cites: Seq[Citation]) {
    println("==============")
    for ((c, idx) <- cites.zipWithIndex) {
      println("[%s] -- %s".format(idx, c.title))
      println("  " + c.authors)
      println("  " + c.conference)
      println()
    }
  }

  def testcite(str: String, shouldAssert: Boolean = true) {
    val parser = new Analysis.CitationParser()
    val cites = parser.parseAll(str)
    if (shouldAssert && cites.isEmpty) {
      log.info("Bad cite: %s", str);
      log.info("Parser log: " + parser.parserLog.mkString("\n"))
      assert(!cites.isEmpty, cites)
    } else {
      printCites(cites)
    }
  }

  test("sinfonia") {
    testcite("""M. K. Aguilera, A. Merchant, M. Shah, A. Veitch, and C. Kara-
manolis. Sinfonia: a new paradigm for building scalable dis-
tributed systems. In SOSP ¦~@~Y07: Proceedings of twenty-¦~Arst ACM 
SIGOPS symposium on Operating systems principles
, pages 159¦~@~S174, New York, NY, USA, 2007. ACM.""")
  }
  
  test("timing") {
    testcite("""
        [2] BARHAM, P., DRAGOVIC, B., FRASER, K., HAND, S.,
HARRIS, T., HO, A., NEUGEBAUER, R., PRATT, I., AND
 WARFIELD, A. Xen and the Art of Virtualization. In SOSP '03:
Proceedings of the nineteenth ACM symposium on Operating systems principles(New York, NY, USA, 2003), ACM, pp. 164�177.
        """)
  }
//
//  test("osdi") {
//    val files = new File("./osdi")
//      .listFiles
//      .filter(_.getName.endsWith(".pdf"))
//      .sortBy(_.getName)
//
//    for (f <- files) {
//      val xml = new String(PDFToXML(new FileInputStream(f), "pdftoxml").get)
//      new File(f.getPath + ".xml").dump(xml)
//      log.info("Processing %s", f)
//      val txt = new Analysis(xml).pages.takeRight(2).flatMap(Analysis.extractText)
//      testcite(txt.mkString(""))
//    }
//  }

  //  test("sequence file") {
  //    println("Here...")
  //    val docs = HadoopUtil
  //      .sequenceFileToStream("file:///home/power/w/autocite/part-00002")
  //      .map({ case (k, v) => Thrift.parseBinary(Document, v).xml })
  //
  //    docs
  //      .filter(_.length > 5000)
  //      .zipWithIndex
  //      .map(v => {
  //        FileUtil.dump(v, "./test-xml." + v._2)
  //        (v._2, new Analysis(v._1).citations)
  //      })
  //      .map(_._2.length < 5)
  //      .map(println)
  //      .length
  //  }

  //
  //  test("MIT Quarterly") {
  //    val a = new Analysis(Source.fromFile("./test-doc.00076.xml").mkString(""))
  //    printCites(a.citations)
  //  }
  //
  //  test("ieee") {
  //    testcite("""C. Gomes, B. Selman, N. Crato, H. Kautz, Heavy-tailed Phenomena in Satisﬁability and Constraint Satisfaction Problems, Journal of Automated Reasoning 24 (1-2) (2000) 67–100.""")
  //    testcite("""M. F. Cowlishaw, “Decimal ﬂoating-point: algorism for computers,” in Proc. of 16th Symposium on Computer Arithmetic, June 2003, pp. 104–111.""")
  //
  //  }
  //
  //  test("atlas") {
  //    testcite("R. Achenbach et al., The ATLAS Level-1 calorimeter trigger, 2008 JINST 3 P03001.")
  //  }
  //
  //  test("springer") {
  //    testcite("Tian, H.T., Huang, L.S., Zhou, Z., et al.: Arm up Administrators: Automated Vulnerability Management." +
  //      "In: Proceedings of the 7th International Symposiumon Parallel Architectures," +
  //      "Algorithms and Networks, Hongkong, China, pp. 587�~@~S593 (2004)")
  //  }
  //
  //  test("assorted1") { testcite("S. Maoz, J. Ringert, and B. Rumpe. Cd2alloy: Class diagrams analysis using alloy revisited. Model Driven Engin eering Languages and Systems, pages 592– 607, 2011. 22 ") }
  //  test("assorted2") { testcite("F. Deissenboeck, B. Hummel, E. Juergens, M. Pfaehler, and B. Schaetz. Model clone detection in practice. In Proceedings of the 4th International Workshop on Software Clones, pages 57–64. ACM, 2010. 6, 19, 27 ") }
  //  test("assorted3") { testcite("P. Godefroid. Compositional dynamic test generation. In POPL, 2007. ") }
  //  test("assorted4") { testcite("Eric S Chung, Peter A Milder, James C Hoe, and Ken Mai. Single-chip heterogeneous computing : Does the future include custom logic , fpgas , and gpgpus ? International Symposium on Microarchitecture (MICRO-43), Atlanta, GA, 2010, pages 225–236, 2010. ") }
  //  test("assorted5") { testcite("C. Teuliere, L. Eck, and E. Marchand. Chasing a moving target from a ﬂying UAV. In Proceedings of International Conference on Intelligent Robots and Systems, pages 4929–4934, Sept. 2011. ") }
  //  test("assorted6") { testcite("Ma X J, Tong J R. Boundary-scan test circuit designed for FPGA. 5th IEEE International Conference on ASIC Proceedings, 2003, 2: 1190 ") }
  //  //  test("assorted7") { testcite("G. Kiczales, E. Hilsdale, J. Hugunin, M. Kersten, J. Palm, and W. G. Griswold. An Overview of AspectJ. In J. L. Knudsen, editor, Proc. ECOOP’01, volume 2072 of LNCS, pages 327–353. Springer, 2001. ") }
  //  test("assorted8") { testcite("Z. Wang, X. Jiang, W. Cui, and P. Ning. Countering Kernel Rootkits with Lightweight Hook Protection. In the Proc. of the Conference on Computer and Communications Security (CCS), 2009. ") }
  //  test("assorted9") { testcite("L. Martignoni, R. Paleari, G. F. Roglia, and D. Bruschi. Testing CPU emulators. In International Symposium on Software Testing and Analysis (ISSTA), 2009. ") }
  //  test("assorted10") { testcite("D.S. Kolovos, D. Di Ruscio, A. Pierantonio, and R.F. Paige. Diﬀerent models for model matching: An analysis of approaches to support model diﬀerencing. In ICSE Workshop on Comparison and Versioning of Software Models, pages 1–6. IEEE, 2009. 9 ") }
  //
  //  test("a") {
  //    var a = new Analysis("""
  //          <doc>
  //          <page number="1">
  //          <text top="1012" left="81" width="88" height="15" font="1">References</text>
  //  <text top="1034" left="87" width="352" height="11" font="2">[1] A. W. Appel, J. R. Ellis, and K. Li. Real-time Concurrent</text>
  //  <text top="1048" left="106" width="334" height="11" font="2">Collection on Stock Multiprocessors. In the Proc. of the Con-</text>
  //  <text top="1061" left="106" width="334" height="11" font="2">ference on Programming Language Design and Implementa-</text>
  //  <text top="1075" left="106" width="105" height="11" font="2">tion (PLDI), 1988.</text>
  //          </page>
  //          </doc>
  //  """)
  //
  //    testcite(a.text)
  //  }
  //  test("watchpoints") {
  //    val a = new Analysis(Common.stringForResource("/paper-00062.xml"))
  //    printCites(a.citations)
  //  }
  //
  //  test("all") {
  //    Common.xmlData
  //      .map(new Analysis(_))
  //      .map(_.citations)
  //      .foreach(printCites _)
  //  }
}
