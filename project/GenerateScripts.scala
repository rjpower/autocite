import sbt._
import Keys._

object GenerateScripts extends Plugin {
  lazy val GenScripts = config("script-gen")

  lazy val scriptUsesJar = SettingKey[Boolean]("script-gen-jar")
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
    scriptUsesJar, 
    fullClasspath in Runtime,
    packageBin in Compile,
    discoveredMainClasses in Compile) map {
      (stream, target, scriptUsesJar, cp, jarFile, scripts) =>
        {
          val separator = java.io.File.pathSeparator
          val log = stream.log
          for (f <- scripts) {
            val scriptName = f.split('.').last
            val targetFile = (target / scriptName).asFile
            
            val jarClassPath = if (scriptUsesJar) { jarFile.getAbsolutePath() + separator } else { "" }
            val classPath = jarClassPath + cp.map(_.data).mkString(separator)
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
