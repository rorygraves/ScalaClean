package scalaclean.cli

import java.io.File
import java.nio.file.Paths

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalafix.testkit.DiffAssertions

abstract class AbstractProjectTestRunner(val projectNames: List[String], overwriteTargetFiles: Boolean) extends DiffAssertions {

  def taskName: String
  def createModelTaskFn(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule

  def run(): Boolean = {

    val propsFiles = projectNames.map { projectName =>
      val srcDir = Paths.get(s"../testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/").toAbsolutePath
      srcDir.resolve(s"ScalaClean.properties").toFile
    }

    val options = SCOptions(taskName,debug = true, validate = true,replace = overwriteTargetFiles,propsFiles)
    val main = new ScalaCleanMain(options,createModelTaskFn(propsFiles, options.debug))
    !main.run()
  }
}
