import sbt._
import Keys._

object GenerateScripts extends Plugin {
  lazy val GenScripts = config("script-gen")

  lazy val genScripts = TaskKey[Unit]("script-gen", "Generate start scripts.")

  lazy val scriptTemplate =
    """#!/bin/bash
    
CLASSPATH="%s"
MAINCLASS="%s"
JVMARGS="-Xms1024m -Xmx3192m"
 
java $JVMARGS -cp $CLASSPATH $MAINCLASS $*

"""

  def genScriptsTask = (
    streams in Runtime,
    target in Runtime,
    fullClasspath in Runtime,
    packageBin in Compile,
    discoveredMainClasses in Compile) map {
      (stream, target, cp, jarFile, scripts) =>
        {
          val separator = java.io.File.pathSeparator
          val log = stream.log
          for (f <- scripts) {
            val scriptName = f.split('.').last
            val targetFile = (target / scriptName).asFile

            val jarPath = jarFile.getAbsolutePath()
            val targetDir = cp.head.data.getAbsolutePath
            val rest = cp.tail.map(_.data.getAbsolutePath)
            val classPathStr = (List(targetDir, jarPath) ++: rest).mkString(":\\\n") 
            log.info("Generating script for %s".format(f))
            IO.write(targetFile, scriptTemplate.format(classPathStr, f))
            targetFile.setExecutable(true)
          }
        }
    }

  val newSettings: Seq[Setting[_]] = Seq(
    genScripts <<= genScriptsTask)
}
