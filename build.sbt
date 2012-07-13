import com.twitter.sbt._

seq((
    Project.defaultSettings ++
    GenerateScripts.newSettings ++
    CompileThriftScrooge.newSettings
): _*)

name          := "autocite"

version       := "1.0"

organization  := "rjpower.org"

scalaVersion  := "2.9.2"
  
scalacOptions ++= Seq("-optimize")

scalacOptions in console ++= Seq("-Xprint:typer")

retrieveManaged := true

compileOrder := CompileOrder.JavaThenScala


resolvers ++= Seq(
  "apache" at "http://maven.apache.org",
  "codahale" at "http://repo.codahale.com",
  "sonatype" at "https://oss.sonatype.org/content/groups/scala-tools/",
  "twitter" at "http://maven.twttr.com"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10",
  "commons-io" % "commons-io" % "2.3",
  "org.apache.hadoop" % "hadoop-core" % "1.0.2",
  "org.apache.lucene" % "lucene-core" % "3.5.0",
  "org.apache.lucene" % "lucene-analyzers" % "3.5.0",
  "org.scalatest" %% "scalatest" % "1.7.2" % "test",
  "org.scalatra" % "scalatra_2.9.1" % "2.+",
  "org.scalatra" % "scalatra-scalate_2.9.1" % "2.+",
  "com.twitter" %% "scrooge-runtime" % "3.0.0",
  "com.twitter" % "finagle-core_2.9.1" % "4.+",
  "com.twitter" % "finagle-thrift_2.9.1" % "4.+", 
  "com.twitter" % "finagle-ostrich4_2.9.1" % "4.+",
  "com.twitter" % "util-core_2.9.1" % "4.+", 
  "com.twitter" % "util-eval_2.9.1" % "4.+",
  "javax.servlet" % "servlet-api" % "2.5" % "provided",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
  )

CompileThriftScrooge.scroogeVersion := "3.0.0"
