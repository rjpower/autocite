package autocite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import com.twitter.logging.{ LoggerFactory, Level, ConsoleHandler }
import com.twitter.logging.Logger

@RunWith(classOf[JUnitRunner])
class ExtractionSpec extends FunSuite with ShouldMatchers {
  private val logConfig = LoggerFactory(
    node = "",
    level = Some(Level.INFO),
    handlers = List(ConsoleHandler()))

  logConfig()
  private val log = Logger.get(getClass)

  //  test("title extraction") {
  //    Common.xmlData
  //      .map(new Analysis(_).title)
  //      .zipWithIndex
  //      .map(f => println(f.swap))
  //  }
  //
  def cites(xml: String) = {
    List(xml)
      .map(new Analysis(_).citations)
      .flatten
  }

  //  test("citation extraction on test set") {
  //    Common.xmlData
  //      .map({
  //        cites(_).map(cite => log.info("      '%s'", cite.title))
  //      })
  //  }
  //
  //  test("citation extraction for oolong") {
  //    cites(Common.stringForResource("/oolong.xml"))
  //      .map(println)
  //  }

  def testcite(str: String, shouldAssert: Boolean = true) {
    val cites = Analysis.CitationParser.parseAll(str)
    if (shouldAssert) { assert(!cites.isEmpty, cites) }
  }

  test("ieee") {
    testcite("""C. Gomes, B. Selman, N. Crato, H. Kautz, Heavy-tailed Phenomena in Satisﬁability and Constraint Satisfaction Problems, Journal of Automated Reasoning 24 (1-2) (2000) 67–100.""")
    testcite("""M. F. Cowlishaw, “Decimal ﬂoating-point: algorism for computers,” in Proc. of 16th Symposium on Computer Arithmetic, June 2003, pp. 104–111.""")

  }

  test("atlas") {
    testcite("R. Achenbach et al., The ATLAS Level-1 calorimeter trigger, 2008 JINST 3 P03001.")
  }

  test("springer") {
    testcite("Tian, H.T., Huang, L.S., Zhou, Z., et al.: Arm up Administrators: Automated Vulnerability Management." +
      "In: Proceedings of the 7th International Symposiumon Parallel Architectures," +
      "Algorithms and Networks, Hongkong, China, pp. 587�~@~S593 (2004)")
  }

  test("assorted1") { testcite("S. Maoz, J. Ringert, and B. Rumpe. Cd2alloy: Class diagrams analysis using alloy revisited. Model Driven Engin eering Languages and Systems, pages 592– 607, 2011. 22 ") }
  test("assorted2") { testcite("F. Deissenboeck, B. Hummel, E. Juergens, M. Pfaehler, and B. Schaetz. Model clone detection in practice. In Proceedings of the 4th International Workshop on Software Clones, pages 57–64. ACM, 2010. 6, 19, 27 ") }
  test("assorted3") { testcite("P. Godefroid. Compositional dynamic test generation. In POPL, 2007. ") }
  test("assorted4") { testcite("Eric S Chung, Peter A Milder, James C Hoe, and Ken Mai. Single-chip heterogeneous computing : Does the future include custom logic , fpgas , and gpgpus ? International Symposium on Microarchitecture (MICRO-43), Atlanta, GA, 2010, pages 225–236, 2010. ") }
  test("assorted5") { testcite("C. Teuliere, L. Eck, and E. Marchand. Chasing a moving target from a ﬂying UAV. In Proceedings of International Conference on Intelligent Robots and Systems, pages 4929–4934, Sept. 2011. ") }
  test("assorted6") { testcite("Ma X J, Tong J R. Boundary-scan test circuit designed for FPGA. 5th IEEE International Conference on ASIC Proceedings, 2003, 2: 1190 ") }
  test("assorted7") { testcite("G. Kiczales, E. Hilsdale, J. Hugunin, M. Kersten, J. Palm, and W. G. Griswold. An Overview of AspectJ. In J. L. Knudsen, editor, Proc. ECOOP’01, volume 2072 of LNCS, pages 327–353. Springer, 2001. ") }
  test("assorted8") { testcite("Z. Wang, X. Jiang, W. Cui, and P. Ning. Countering Kernel Rootkits with Lightweight Hook Protection. In the Proc. of the Conference on Computer and Communications Security (CCS), 2009. ") }
  test("assorted9") { testcite("L. Martignoni, R. Paleari, G. F. Roglia, and D. Bruschi. Testing CPU emulators. In International Symposium on Software Testing and Analysis (ISSTA), 2009. ") }
  test("assorted10") { testcite("D.S. Kolovos, D. Di Ruscio, A. Pierantonio, and R.F. Paige. Diﬀerent models for model matching: An analysis of approaches to support model diﬀerencing. In ICSE Workshop on Comparison and Versioning of Software Models, pages 1–6. IEEE, 2009. 9 ") }

  test("all") {
    def printCite(cites : Seq[Citation]) {
      println("==============")
      for (c <- cites) {
        println(c.title)
        println("  " + c.authors)
        println("  " + c.conference)
        println()
      }
    }

    Common.xmlData
      .map(new Analysis(_))
      .map(_.citations)
      .foreach(printCite _)
  }


  //  test("dump possible citations") {
  //    Common.xmlData
  //      .map(new Analysis(_))
  //      .map(n => Analysis.possibleCitations(n.nodes))
  //      .flatten
  //      .map(_.replaceAll("\n", " "))
  //      .map(t => """testcite("%s")""".format(t))
  //      .map(println)
  //  }

}
