package autocite

import java.io.{ObjectOutputStream, ObjectInputStream}
import java.net.URLClassLoader
import scala.Array.canBuildFrom
import scala.collection.JavaConversions._
import scala.collection.Iterator
import scala.math.{pow, log1p, log}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.hadoop.io.compress.GzipCodec
import org.apache.hadoop.io._
import org.apache.hadoop.mapred.lib.MultipleInputs
import org.apache.hadoop.mapred._
import com.codahale.logula.Logging
import com.twitter.scrooge.{ThriftStructCodec, ThriftStruct}
import java.io.ByteArrayInputStream
import org.apache.commons.io.output.ByteArrayOutputStream


object HadoopImplicits {
  implicit def writable2boolean(value: BooleanWritable) = value.get
  implicit def boolean2writable(value: Boolean) = new BooleanWritable(value)

  implicit def writable2int(value: IntWritable) = value.get
  implicit def int2writable(value: Int) = new IntWritable(value)

  implicit def writable2long(value: LongWritable) = value.get
  implicit def long2writable(value: Long) = new LongWritable(value)

  implicit def writable2float(value: FloatWritable) = value.get
  implicit def float2writable(value: Float) = new FloatWritable(value)

  implicit def bytes2writable(value: Array[Byte]) = new BytesWritable(value)
  implicit def writable2bytes(value: BytesWritable): Array[Byte] = value.getBytes
}

abstract class ScalaBase[KO, VO] extends MapReduceBase with Logging {
  var reporter: Reporter = null
  var context: OutputCollector[KO, VO] = null

  def emit(key: KO, value: VO) {
    context.collect(key, value)
  }

  def increment(name: String) {
    reporter.getCounter("Autocite", name).increment(1)
  }

  def histogram(name: String, value: Int) = {
    val bucket = pow(1.1, log1p(value) / scala.math.log(1.1)).toInt
    reporter.getCounter("Autocite", name + "-bucket-%d".format(bucket)).increment(1)
  }
}

abstract class ScalaMapper[K, V, KO, VO]
  extends ScalaBase[KO, VO] with Mapper[K, V, KO, VO] {

  final def map(key: K, value: V, ctx: OutputCollector[KO, VO], r: Reporter) {
    reporter = r
    context = ctx
    _map(key, value)
  }

  def _map(key: K, value: V): Unit

  // Set the default input format to be sequence files.
  def inputFormat: Class[_ <: InputFormat[K, V]] = classOf[SequenceFileInputFormat[K, V]]
}

abstract class ScalaReducer[K, V, KO, VO]
  extends ScalaBase[KO, VO] with Reducer[K, V, KO, VO] {
  final def reduce(key: K, values: java.util.Iterator[V],
    ctx: OutputCollector[KO, VO], r: Reporter) {
    reporter = r
    context = ctx
    _reduce(key, values)
  }

  def _reduce(key: K, values: Iterator[V])
  def outputFormat: Class[_ <: OutputFormat[KO, VO]] = classOf[SequenceFileOutputFormat[KO, VO]]
}

class GenericMapper[K, V, KO <: Writable, VO <: Writable] extends ScalaMapper[K, V, KO, VO] {
  var mapFunction: (K, V) => (KO, VO) = null

  override def configure(conf: JobConf) {
    val serializedMapper = conf.get("mapperFunction")
    mapFunction =
      new ObjectInputStream(
        new ByteArrayInputStream(serializedMapper.getBytes()))
        .readObject()
        .asInstanceOf[(K, V) => (KO, VO)]
  }

  def _map(key: K, value: V) {
    val (kOut, vOut) = mapFunction(key, value)
    emit(kOut, vOut)
  }
}

object HadoopUtil {
  def sequenceFileWriter[T](outputFile: String)(implicit m: Manifest[T]) = {
    val conf = new Configuration()
    val outputPath = new Path(outputFile)
    new SequenceFile.Writer(
      outputPath.getFileSystem(conf),
      conf, outputPath, classOf[LongWritable], m.erasure)
  }

  def listToSequenceFile[T](l: Seq[T], outputFile: String)(implicit m: Manifest[T]) {
    val writer = sequenceFileWriter[T](outputFile)
    for (v <- l) {
      writer.append(new LongWritable(0), v)
    }

    writer.close()
  }

  def sequenceFileReader[T](fileName: String) = {
    new SequenceFileRecordReader[LongWritable, T](
      new Configuration(),
      new FileSplit(
        new Path(fileName),
        0, 1000 * 1000 * 1000, null.asInstanceOf[Array[String]]))
  }

  def sequenceFileToStream(fileName: String): Iterator[(Long, Array[Byte])] = {
    new Iterator[(Long, Array[Byte])] {
      val reader = sequenceFileReader[BytesWritable](fileName)
      var kOut: Long = 0
      var vOut: Array[Byte] = null

      val kIn = new LongWritable
      val vIn = new BytesWritable

      var hasNext = reader.next(kIn, vIn)
      def next = {
        kOut = kIn.get
        vOut = vIn.getBytes
        hasNext = reader.next(kIn, vIn)
        (kOut, vOut)
      }
    }
  }
}

class ContextHelper {
  val job = new JobConf
  val loader = classOf[ContextHelper].getClassLoader().asInstanceOf[URLClassLoader]

  val userLibs = loader.getURLs()
    .filter(a => !a.getPath.startsWith("file:/usr/lib"))
    .map(a => a.toString())
    .mkString(",")
  job set ("tmpjars", userLibs)

  job setNumReduceTasks 100
  job setMaxMapAttempts 10
  job setMaxReduceAttempts 10

  FileOutputFormat.setCompressOutput(job, true)
  FileOutputFormat.setOutputCompressorClass(job, classOf[GzipCodec])
  job setMapOutputCompressorClass (classOf[GzipCodec])

  def map[MKeyIn, MValueIn, MKeyOut <: Writable, MValueOut <: Writable](
    mapper: (MKeyIn, MValueIn) => (MKeyOut, MValueOut), in: String) = {
    val bytes = new ByteArrayOutputStream()
    val obj = new ObjectOutputStream(bytes)
    obj.writeObject(mapper)
    obj.close
    
    job.set("serializedMapper", new String(bytes.toByteArray))
    
    val m = new GenericMapper[MKeyIn, MValueIn, MKeyOut, MValueOut]
    MultipleInputs.addInputPath(
      job, new Path(in), m.inputFormat, m.getClass())
    this
  }

  def mapper[MKeyIn, MValueIn, MKeyOut, MValueOut](
    mapper: Class[_ <: ScalaMapper[MKeyIn, MValueIn, MKeyOut, MValueOut]],
    in: String) = {
    MultipleInputs.addInputPath(
      job, new Path(in), mapper.newInstance().inputFormat, mapper)
    this
  }

  def reducer[MKeyOut, MValueOut, RKeyOut, RValueOut](
    reducer: Class[_ <: ScalaReducer[MKeyOut, MValueOut, RKeyOut, RValueOut]],
    out: String)(
      implicit mapKeyOutManifest: Manifest[MKeyOut],
      mapValueOutManifest: Manifest[MValueOut],
      reduceKeyOutManifest: Manifest[RKeyOut],
      reduceValueOutManifest: Manifest[RValueOut]) = {
    job setReducerClass reducer
    job setOutputKeyClass reduceKeyOutManifest.erasure
    job setOutputValueClass reduceValueOutManifest.erasure
    job setMapOutputValueClass mapValueOutManifest.erasure
    job setMapOutputKeyClass mapKeyOutManifest.erasure
    job setOutputFormat reducer.newInstance().outputFormat

    job.set("mapred.output.compression.type", "BLOCK")

    val output = new Path(out)
    val fs = output.getFileSystem(job)
    fs.delete(output, true)

    FileOutputFormat setOutputPath (job, output)
    this
  }

  def local() = {
    job.set("mapred.job.tracker", "local")
    this
  }

  def run() = {
    JobClient runJob job
  }
}
