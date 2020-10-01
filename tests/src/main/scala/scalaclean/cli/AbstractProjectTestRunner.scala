package scalaclean.cli

import java.io.File
import java.nio.file.Paths

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalaclean.util.DiffAssertions

abstract class AbstractProjectTestRunner(val projectNames: List[String], runOptions: SimpleRunOptions)
    extends DiffAssertions {

  def taskName: String

  def createModelTaskFn(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule

  def run(): Boolean = {

    // sbt and intellij have different ideas about the base directory for running tests.
    // so try both options
    val propsFiles = projectNames.map { projectName =>
      val srcDir =
        Paths.get(s"../testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/").toAbsolutePath
      val f1 = srcDir.resolve(s"ScalaClean.properties").toFile
      if (f1.exists()) {
        f1
      } else {
        val srcDir =
          Paths.get(s"testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/").toAbsolutePath
        val f1 = srcDir.resolve(s"ScalaClean.properties").toFile
        f1
      }

    }

    val options = SCOptions(
      taskName,
      debug = runOptions.debug,
      addComments = runOptions.addComments,
      validate = true,
      replace = runOptions.replace,
      propsFiles
    )
    val main = new ScalaCleanMain(options, createModelTaskFn(propsFiles, options.debug))
    !main.run() || options.replace
  }

}
