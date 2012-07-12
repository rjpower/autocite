package autocite

import org.apache.hadoop.io.{ LongWritable, BytesWritable }
import org.apache.lucene.document.Field.{ Store, Index }
import org.apache.lucene.document.{ Field, Document => LuceneDoc }

import autocite.HadoopImplicits.{ writable2bytes, long2writable, bytes2writable }
import autocite.util._
import autocite.util.Implicits._
import edu.cmu.lemurproject.WarcFileInputFormat
import edu.cmu.lemurproject.WritableArchiveRecord

class PDFToDocumentMapper extends ScalaMapper[LongWritable, WritableArchiveRecord, LongWritable, BytesWritable] {
  override def inputFormat = classOf[WarcFileInputFormat]
  def _map(key: LongWritable, value: WritableArchiveRecord): Unit = {
    val header = value.data.getHeader()

    if (header.getUrl() == null || header.getUrl() == "") {
      increment("MissingUrl")
      return
    }

    val url = header.getUrl().toLowerCase()
    log.info("Working on %s", url)
    if (header.getLength() < 5000) {
      log.info("Too small: %s", url)
      increment("TooSmall")
      return
    }

    var filetype = ""
    if (url.contains(".pdf")) {
      log.info("PDF: %s", url)
      filetype = "PDF"
    } else if (url.contains(".ps")) {
      log.info("Postcript: %s", url)
      filetype = "PostScript"
    } else {
      log.info("Unknown: %s", url)
      filetype = "Unknown-PS"
    }

    val xml = PDFToXML(value)
    if (xml != null) {
      increment(filetype + ".success")
      emit(xml.hashCode, Document(
        xml = new String(xml),
        url = url,
        title = "",
        incoming = List(), outgoing = List(), text = "").binary)
    } else {
      increment(filetype + ".failure")
    }
  }
}

class ExtractInfoMapper extends ScalaMapper[LongWritable, BytesWritable, LongWritable, BytesWritable] {
  def _map(key: LongWritable, value: BytesWritable): Unit = {
    if (value.getLength > 10000000) {
      increment("TooLong")
      return
    }

    val doc = Thrift.parseBinary(Document, value.getBytes)
    log.info("Processing... %s", doc.url)
    val analyzer = new Analysis(doc.xml)
    val title = analyzer.title
    if (title.isEmpty) {
      increment("NoTitle")
    } else if (title.length < 5) {
      increment("ShortTitle")
    } else {
      increment("GoodTitle")
      val md = Document(
        url = doc.url,
        title = title,
        incoming = List(),
        outgoing = analyzer.citations,
        xml = doc.xml,
        text = analyzer.text)
      emit(analyzer.id, md.binary)
    }
  }
}

class SelectTestSetMapper extends ScalaMapper[LongWritable, BytesWritable, LongWritable, BytesWritable] {
  def _map(key: LongWritable, value: BytesWritable) {
    val doc = Thrift.parseBinary(Document, value.getBytes)
    val analyzer = new Analysis(doc.xml)
    doc.incoming.map(c =>
      emit(Analysis.titleToId(c.title), CiteOrDoc(cite = Some(c), doc = None).binary))
    emit(analyzer.id, CiteOrDoc(cite = None, doc = Some(doc)).binary)
  }
}

class SelectTestSetReducer extends ScalaReducer[LongWritable, BytesWritable, LongWritable, BytesWritable] {
  def _reduce(key: LongWritable, values: Iterator[BytesWritable]): Unit = {
    val entries = values.take(200).map(bw => Thrift.parseBinary(CiteOrDoc, bw)).toList
    val cites = entries.filter(_.cite.isDefined).map(_.cite.get)
    val docs = entries.filter(_.doc.isDefined).map(_.doc.get)

    if (docs.isEmpty) {
      increment("Missing Document")
      return
    }

    if (cites.length < 20) {
      increment("Too Few Citations")
      return
    }

    if (cites.length > 100) {
      increment("Too Many Citations")
      return
    }

    def lenSort(d: Document): Long = d.xml.length
    val best = docs.sortBy(lenSort).last

    emit(Analysis.titleToId(best.title), best.binary)
  }
}

class InvertCitationsMapper extends ScalaMapper[LongWritable, BytesWritable, LongWritable, BytesWritable] {
  def _map(key: LongWritable, value: BytesWritable) {
    val doc = Thrift.parseBinary(Document, value.getBytes)
    val analyzer = new Analysis(doc.xml)
    histogram("map-citations", doc.outgoing.length)
    doc.outgoing.map(c =>
      emit(Analysis.titleToId(c.title), CiteOrDoc(cite = Some(c), doc = None).binary))
    emit(
      analyzer.id, CiteOrDoc(cite = None, doc = Some(doc)).binary)
  }
}

class InvertCitationsReducer extends ScalaReducer[LongWritable, BytesWritable, LongWritable, BytesWritable] {
  def _reduce(key: LongWritable, values: Iterator[BytesWritable]) {
    val entries = values.take(1000).map(
      bw => Thrift.parseBinary(CiteOrDoc, bw.getBytes)).toList

    val cites = entries.filter(_.cite.isDefined).map(_.cite.get)
    val documents = entries.filter(_.doc.isDefined).map(_.doc.get)

    histogram("citation.count", cites.length)
    histogram("doc.count", documents.length)

    if (documents.isEmpty)
      return

    // Pick the largest document if there was more then one with the same title.
    def lenSort(d: Document): Long = d.xml.length
    val best = documents.sortBy(lenSort).last
    val combined = Document(
      url = best.url,
      title = best.title,
      xml = best.xml,
      incoming = cites,
      outgoing = best.outgoing,
      text = best.text)
    emit(Analysis.titleToId(best.title), combined.binary)
  }
}

class LuceneMapper extends ScalaMapper[LongWritable, BytesWritable, LongWritable, BytesWritable] {
  def _map(key: LongWritable, value: BytesWritable) {
    emit(key, value)
  }
}

class LuceneReducer extends ScalaReducer[LongWritable, BytesWritable, LongWritable, LuceneDoc] {
  override def outputFormat = classOf[LuceneOutputFormat]

  def field(name: String, value: String, index: Field.Index = Index.ANALYZED) = {
    new Field(name, value, Store.YES, index)
  }

  def field(name: String, value: Array[Byte]) = {
    new Field(name, value)
  }

  def _reduce(key: LongWritable, values: Iterator[BytesWritable]) {
    val doc = Thrift.parseBinary(Document, values.next)
    val luceneDoc = new LuceneDoc()
    val analyzer = new Analysis(doc.xml)
    luceneDoc.add(field("binary", Compress.compress(doc.binary)))
    luceneDoc.add(field("text", analyzer.text))
    luceneDoc.add(field("title", doc.title))
    luceneDoc.add(field("references", doc.incoming.length.toString))
    luceneDoc.add(field("docid", analyzer.id.toString))
    emit(analyzer.id, luceneDoc)
  }
}

class DocToXmlMapper extends ScalaMapper[LongWritable, BytesWritable, LongWritable, BytesWritable] {
  def _map(key: LongWritable, value: BytesWritable) = {
    emit(key, Thrift.parseBinary(Document, value).xml.getBytes)
  }
}

class IdentityMapper[K, V] extends ScalaMapper[K, V, K, V] {
  def _map(key: K, value: V) {
    emit(key, value)
  }
}

class IdentityReducer[KIn, VIn] extends ScalaReducer[KIn, VIn, KIn, VIn] {
  def _reduce(key: KIn, values: Iterator[VIn]) {
    for (v <- values) { emit(key, v) }
  }
}

object Indexer {
  def pdfToDocument() = {
    new ContextHelper()
      .mapper(classOf[PDFToDocumentMapper], "/autocite-epoch-*/warcs/*.gz")
      .reducer(classOf[IdentityReducer[LongWritable, BytesWritable]], "/autocite/xml")
      .run()
  }

  def extractDocumentInfo() = {
    new ContextHelper()
      .mapper(classOf[ExtractInfoMapper], "/autocite/xml/part*")
      .reducer(classOf[IdentityReducer[LongWritable, BytesWritable]], "/autocite/document")
      .run()
  }

  def invertCitations() = {
    new ContextHelper()
      .mapper(classOf[InvertCitationsMapper], "/autocite/document/*")
      .reducer(classOf[InvertCitationsReducer], "/autocite/doc-plus-cites")
      .run()
  }

  def buildIndex() = {
    new ContextHelper()
      .mapper(classOf[LuceneMapper], "/autocite/doc-plus-cites/part*")
      .reducer(classOf[LuceneReducer], "/autocite/index")
      .run()
  }

  def selectTestSet() = {
    new ContextHelper()
      .mapper(classOf[SelectTestSetMapper], "/autocite/doc-plus-cites/part*")
      .reducer(classOf[SelectTestSetReducer], "/autocite/test-set")
      .run()
  }

  def main(args: Array[String]): Unit = {
    //pdfToDocument()
        extractDocumentInfo()
    // invertCitations()
    // buildIndex()

    //selectTestSet()
  }
}
