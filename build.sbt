import com.twitter.sbt._

seq((
    Project.defaultSettings ++
    GenerateScripts.newSettings ++
    CompileThriftScrooge.newSettings
): _*)

name          := "autocite"

version       := "1.0"

organization  := "rjpower.org"

scalaVersion  := "2.9.1"
  
scalacOptions ++= Seq("-explaintypes", "-Xexperimental", "-deprecation")

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
  "com.codahale" %% "logula" % "2.1.3",
  "org.scalatra" %% "scalatra" % "2.0.4",
  "org.scalatra" %% "scalatra-scalate" % "2.0.4",
  "com.twitter" %% "scrooge-runtime" % "1.1.3",
  "com.twitter" %% "finagle-core" % "4.0.2",
  "com.twitter" %% "finagle-thrift" % "4.0.2", 
  "com.twitter" %% "finagle-ostrich4" % "4.0.2",
  "com.twitter" %% "util-core" % "4.0.1", 
  "com.twitter" %% "util-eval" % "4.0.1",
  "javax.servlet" % "servlet-api" % "2.5" % "provided",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
  )

CompileThriftScrooge.scroogeVersion := "2.5.4"
