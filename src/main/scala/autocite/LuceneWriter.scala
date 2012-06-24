package autocite
import java.io.File
import org.apache.hadoop.fs.{Path, FileSystem}
import org.apache.hadoop.io.LongWritable
import org.apache.hadoop.mapred.{Reporter, RecordWriter, OutputFormat, JobConf, FileOutputFormat}
import org.apache.hadoop.util.Progressable
import org.apache.lucene.analysis.shingle.ShingleAnalyzerWrapper
import org.apache.lucene.document.{Document => LuceneDoc}
import org.apache.lucene.index.{IndexWriterConfig, IndexWriter}
import org.apache.lucene.store.FSDirectory
import com.twitter.logging.Logger

class LuceneRecordWriter(val fs: FileSystem, val job: JobConf, val name: String) extends RecordWriter[LongWritable, LuceneDoc] {
  val log = Logger.get(classOf[LuceneRecordWriter])
  
  val version = org.apache.lucene.util.Version.LUCENE_35
  val indexDirectory = FSDirectory.open(new java.io.File("./tmp/"))
  val indexConf = new IndexWriterConfig(version, new ShingleAnalyzerWrapper(version, 2, 4))
  val indexWriter = new IndexWriter(indexDirectory, indexConf)

  def write(key: LongWritable, value: LuceneDoc) {
    indexWriter.addDocument(value)
  }

  def close(reporter: Reporter) {
    indexWriter.forceMerge(1)
    indexWriter.close()

    val sourceDir = indexDirectory.getDirectory()

    for (indexFile <- indexDirectory.listAll()) {
      log.info("Copying local file %s".format(indexFile))
      fs.copyFromLocalFile(
        new Path(new File(sourceDir, indexFile).getAbsolutePath()),
        FileOutputFormat.getTaskOutputPath(job, name + "/" + indexFile))
    }

    log.info("Finished close.")
  }
}

class LuceneOutputFormat extends OutputFormat[LongWritable, LuceneDoc] {
  def checkOutputSpecs(fs: FileSystem, job: JobConf) = {}

  def getRecordWriter(fs: FileSystem, job: JobConf, name: String, progress: Progressable) = {
    new LuceneRecordWriter(fs, job, name)
  }
}
