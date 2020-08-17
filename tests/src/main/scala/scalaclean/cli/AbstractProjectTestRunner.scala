package scalaclean.cli

import java.io.File
import java.nio.file.Files

import scalaclean.cli.AbstractProjectTestRunner.testProjectPropsFile
import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalafix.testkit.DiffAssertions

import scala.meta.io.{ AbsolutePath, RelativePath }

object AbstractProjectTestRunner {
  // sbt & IJ have different ideas about the base dir for running tests, so try both options
  lazy val testProjectsDir: RelativePath = {
    val dir1 = RelativePath("../testProjects")
    if (Files.exists(dir1.toNIO)) dir1 else RelativePath("testProjects")
  }

  def testProjectPropsFile(projectName: String): AbsolutePath = {
    val rel = s"$projectName/target/scala-2.12/classes/META-INF/ScalaClean/ScalaClean.properties"
    testProjectsDir.resolve(rel).toAbsolute
  }
}

abstract class AbstractProjectTestRunner(
    val projectNames: List[String], overwriteTargetFiles: Boolean) extends DiffAssertions {

  def taskName: String

  def createRule(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule

  def run(): Boolean = {
    val propsFiles = projectNames.map(testProjectPropsFile(_).toFile)
    val options = SCOptions(taskName, debug = true, validate = true, replace = overwriteTargetFiles, propsFiles)
    val main = new ScalaCleanMain(options, createRule(propsFiles, options.debug))
    !main.run() || overwriteTargetFiles
  }
}
