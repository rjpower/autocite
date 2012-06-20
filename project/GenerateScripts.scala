import sbt._
import Keys._

object GenerateScripts extends Plugin {
  lazy val GenScripts = config("genscripts")

  lazy val scriptUsesJar = SettingKey[Boolean]("use-jar-in-classpath")
  lazy val genScripts = TaskKey[Unit]("generate-scripts", "Generate start scripts.")

  lazy val scriptTemplate =
    """#!/bin/bash
    
CLASSPATH="%s"
MAINCLASS="%s"
JVMARGS="-Xmx12000m"
 
java $JVMARGS -cp $CLASSPATH $MAINCLASS

"""

  def genScriptsTask = (
    streams in Runtime,
    target in Runtime, 
    scriptUsesJar, 
    fullClasspath in Runtime,
    packageBin in Compile,
    discoveredMainClasses in Compile) map {
      (stream, target, useJarFile, cp, jarFile, scripts) =>
        {
          val log = stream.log
          for (f <- scripts) {
            val scriptName = f.split('.').last
            val targetFile = (target / scriptName).asFile
            val jarClassPath = if (useJarFile) { jarFile.getAbsolutePath() + ":" } else { "" }
            val classPath = jarClassPath + cp.map(_.data).mkString(":")
            log.info("Generating script for %s".format(f))
            IO.write(targetFile, scriptTemplate.format(classPath, f))
            targetFile.setExecutable(true)
          }
        }
    }

  val newSettings: Seq[Setting[_]] = Seq(
    scriptUsesJar := true,
    genScripts <<= genScriptsTask)
}
