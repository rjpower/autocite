package autocite
import scala.io.Source
import com.codahale.logula.Logging
import scala.io.Codec

object Common extends Logging {
  def stripDtd(item: String) = {
    // Strip DTD lines from the document
    val dtdRe = "<!DOCTYPE pdf2xml[^>]+>".r
    dtdRe.replaceAllIn(item, "")
  }

  def stringForResource(r: String) = {
    val is = getClass.getResourceAsStream(r)
    new String(Source.fromInputStream(is, Codec.UTF8.name).toArray[Char])
  }

  lazy val xmlData = (for (i <- Range(1, 100)) yield {
    stringForResource("/paper-%05d.xml".format(i))
  }).toList.map(stripDtd)

  val sampleReferences =
    <pdf2xml>
      <page number="12" position="absolute" top="0" left="0" height="1188" width="918">
        <text top="606" left="121" width="197" height="16" font="5">ACKNOWLEDGMENTS</text>
        <text top="628" left="94" width="345" height="12" font="6">We thank Shekhar Borkar for sharing his personal views on how</text>
        <text top="644" left="81" width="359" height="12" font="6">CMOS devices are likely to scale. Support for this research was</text>
        <text top="660" left="81" width="359" height="12" font="6">provided by NSF under the following grants: CCF-0845751, CCF-</text>
        <text top="675" left="81" width="359" height="12" font="6">0917238, and CNS-0917213. Any opinions, ﬁndings, and conclu-</text>
        <text top="691" left="81" width="359" height="12" font="6">sions or recommendations expressed in this material are those of</text>
        <text top="707" left="81" width="319" height="12" font="6">the authors and do not necessarily reﬂect the views of NSF.</text>
        <text top="733" left="81" width="83" height="16" font="5">References</text>
        <text top="758" left="87" width="352" height="12" font="6">[1] G. M. Amdahl. Validity of the single processor approach to</text>
        <text top="774" left="110" width="322" height="12" font="6">achieving large-scale computing capabilities. In AFIPS 67.</text>
        <text top="793" left="87" width="352" height="12" font="6">[2] O. Azizi, A. Mahesri, B. C. Lee, S. J. Patel, and M. Horowitz.</text>
        <text top="808" left="110" width="150" height="12" font="6">Energy-performance tradeo</text>
        <text top="805" left="260" width="180" height="17" font="6">s in processor architecture and</text>
        <text top="824" left="110" width="285" height="12" font="6">circuit design: a marginal cost analysis. In ISCA 10.</text>
        <text top="843" left="87" width="352" height="12" font="6">[3] A. Bakhoda, G. L. Yuan, W. W. L. Fung, H. Wong, and T. M.</text>
        <text top="858" left="110" width="330" height="12" font="6">Aamodt. Analyzing CUDA workloads using a detailed GPU</text>
        <text top="874" left="110" width="137" height="12" font="6">simulator. In ISPASS 09.</text>
        <text top="893" left="87" width="260" height="12" font="6">[4] M. Bhadauria, V. Weaver, and S. McKee.</text>
        <text top="893" left="360" width="79" height="12" font="6">Understanding</text>
        <text top="908" left="110" width="330" height="12" font="6">PARSEC performance on contemporary CMPs. In IISWC 09.</text>
        <text top="927" left="87" width="352" height="12" font="6">[5] C. Bienia, S. Kumar, J. P. Singh, and K. Li. The PARSEC</text>
        <text top="943" left="110" width="330" height="12" font="6">benchmark suite: Characterization and architectural implica-</text>
        <text top="959" left="110" width="105" height="12" font="6">tions. In PACT 08.</text>
        <text top="977" left="87" width="352" height="12" font="6">[6] S. Borkar. Thousand core chips: a technology perspective. In</text>
        <text top="993" left="110" width="47" height="12" font="6">DAC 07</text>
        <text top="993" left="157" width="3" height="12" font="6">.</text>
        <text top="1012" left="87" width="352" height="12" font="6">[7] S. Borkar. The exascale challenge. Keynote at International</text>
        <text top="1027" left="110" width="330" height="12" font="6">Symposium on VLSI Design, Automation and Test (VLSI-</text>
        <text top="1043" left="110" width="67" height="12" font="6">DAT), 2010.</text>
        <text top="87" left="482" width="352" height="12" font="6">[8] K. Chakraborty. Over-provisioned Multicore Systems. PhD</text>
        <text top="102" left="504" width="256" height="12" font="6">thesis, University of Wisconsin-Madison, 2008.</text>
        <text top="121" left="482" width="352" height="12" font="6">[9] S. Cho and R. Melhem. Corollaries to Amdahls law for en-</text>
        <text top="137" left="504" width="306" height="12" font="6">ergy. Computer Architecture Letters, 7(1), January 2008.</text>
        <text top="155" left="475" width="359" height="12" font="6">[10] E. S. Chung, P. A. Milder, J. C. Hoe, and K. Mai. Single-</text>
        <text top="171" left="504" width="330" height="12" font="6">chip heterogeneous computing: Does the future include cus-</text>
        <text top="187" left="504" width="252" height="12" font="6">tom logic, FPGAs, and GPUs? In MICRO 10.</text>
        <text top="205" left="475" width="359" height="12" font="6">[11] R. H. Dennard, F. H. Gaensslen, V. L. Rideout, E. Bassous,</text>
        <text top="221" left="504" width="330" height="12" font="6">and A. R. LeBlanc. Design of ion-implanted mosfets with</text>
        <text top="237" left="504" width="330" height="12" font="6">very small physical dimensions. IEEE Journal of Solid-State</text>
        <text top="253" left="504" width="43" height="12" font="6">Circuits</text>
        <text top="253" left="547" width="97" height="12" font="6">, 9, October 1974.</text>
        <text top="271" left="475" width="359" height="12" font="6">[12] H. Esmaeilzadeh, T. Cao, Y. Xi, S. M. Blackburn, and K. S.</text>
        <text top="287" left="504" width="330" height="12" font="6">McKinley. Looking back on the language and hardware rev-</text>
        <text top="303" left="504" width="330" height="12" font="6">olutions: measured power, performance, and scaling. In AS-</text>
        <text top="318" left="504" width="53" height="12" font="6">PLOS 11</text>
        <text top="318" left="558" width="3" height="12" font="6">.</text>
        <text top="337" left="475" width="359" height="12" font="6">[13] Z. Guz, E. Bolotin, I. Keidar, A. Kolodny, A. Mendelson, and</text>
        <text top="353" left="504" width="330" height="12" font="6">U. C. Weiser. Many-core vs. many-thread machines: Stay</text>
        <text top="368" left="504" width="330" height="12" font="6">away from the valley. IEEE Computer Architecture Letters,</text>
        <text top="384" left="504" width="89" height="12" font="6">8, January 2009.</text>
        <text top="403" left="475" width="359" height="12" font="6">[14] M. Hempstead, G.-Y. Wei, and D. Brooks. Navigo: An early-</text>
        <text top="419" left="504" width="330" height="12" font="6">stage model to study power-contrained architectures and spe-</text>
        <text top="434" left="504" width="136" height="12" font="6">cialization. In MoBS 09.</text>
        <text top="453" left="475" width="359" height="12" font="6">[15] M. D. Hill and M. R. Marty. Amdahls law in the multicore</text>
        <text top="469" left="504" width="177" height="12" font="6">era. Computer, 41(7), July 2008.</text>
        <text top="487" left="475" width="237" height="12" font="6">[16] M. Horowitz, E. Alon, D. Patil, S. Na</text>
        <text top="484" left="712" width="122" height="17" font="6">ziger, R. Kumar, and</text>
        <text top="503" left="504" width="330" height="12" font="6">K. Bernstein. Scaling, power, and the future of CMOS. In</text>
        <text top="519" left="504" width="55" height="12" font="6">IEDM 05</text>
        <text top="519" left="559" width="3" height="12" font="6">.</text>
        <text top="537" left="475" width="359" height="12" font="6">[17] E. Ipek, M. Kirman, N. Kirman, and J. F. Martinez. Core</text>
        <text top="553" left="504" width="330" height="12" font="6">fusion: accommodating software diversity in chip multipro-</text>
        <text top="569" left="504" width="115" height="12" font="6">cessors. In ISCA 07.</text>
        <text top="588" left="475" width="359" height="12" font="6">[18] ITRS. International technology roadmap for semiconductors,</text>
        <text top="603" left="504" width="276" height="12" font="6">2010 update, 2011. URL http://www.itrs.net.</text>
        <text top="622" left="475" width="359" height="12" font="6">[19] C. Kim, S. Sethumadhavan, M. S. Govindan, N. Ranganathan,</text>
        <text top="638" left="504" width="245" height="12" font="6">D. Gulati, D. Burger, and S. W. Keckler.</text>
        <text top="638" left="767" width="67" height="12" font="6">Composable</text>
        <text top="653" left="504" width="211" height="12" font="6">lightweight processors. In MICRO 07.</text>
        <text top="672" left="475" width="220" height="12" font="6">[20] J.-G. Lee, E. Jung, and W. Shin.</text>
        <text top="672" left="709" width="125" height="12" font="6">An asymptotic perfor-</text>
        <text top="688" left="504" width="35" height="12" font="6">mance</text>
        <text top="684" left="539" width="294" height="17" font="6">/energy analysis and optimization of multi-core archi-</text>
        <text top="703" left="504" width="137" height="12" font="6">tectures. In ICDCN 09, .</text>
        <text top="722" left="475" width="359" height="12" font="6">[21] V. W. Lee et al. Debunking the 100X GPU vs. CPU myth:</text>
        <text top="738" left="504" width="330" height="12" font="6">an evaluation of throughput computing on CPU and GPU. In</text>
        <text top="754" left="504" width="50" height="12" font="6">ISCA 10</text>
        <text top="754" left="554" width="10" height="12" font="6">, .</text>
        <text top="772" left="475" width="359" height="12" font="6">[22] G. Loh. The cost of uncore in throughput-oriented many-core</text>
        <text top="788" left="504" width="135" height="12" font="6">processors. In ALTA 08.</text>
        <text top="807" left="475" width="359" height="12" font="6">[23] G. E. Moore. Cramming more components onto integrated</text>
        <text top="822" left="504" width="213" height="12" font="6">circuits. Electronics, 38(8), April 1965.</text>
        <text top="841" left="475" width="359" height="12" font="6">[24] K. Nose and T. Sakurai. Optimization of VDD and VTH for</text>
        <text top="857" left="504" width="311" height="12" font="6">low-power and high speed applications. In ASP-DAC 00.</text>
        <text top="875" left="475" width="359" height="12" font="6">[25] SPEC. Standard performance evaluation corporation, 2011.</text>
      </page>
    </pdf2xml>
}