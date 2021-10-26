package scalaclean.cli

import java.io.File
import java.nio.file.{Files, Path, Paths}

import scalaclean.model.{AllProjectsModel, NotNothing}
import scalaclean.model.impl.ProjectSet
import scalaclean.rules.RuleRun
import scalaclean.util.DiffAssertions
import org.scalatest.Assertions

import scala.reflect.{ClassTag, classTag}
import scala.jdk.CollectionConverters._

abstract class AbstractProjectTestRunner[Cmd <: ScalaCleanCommandLine: ClassTag: NotNothing, Rule <: RuleRun[Cmd]](val projectNames: List[String], runOptions: SimpleRunOptions)
    extends DiffAssertions with Assertions{

  def cmdLine: Cmd = classTag[Cmd].runtimeClass.newInstance().asInstanceOf[Cmd]
  def rule(cmd: Cmd, model: AllProjectsModel): Rule

  def customise(options: Cmd): Unit = ()

  def run(): Unit = {

    // sbt and intellij have different ideas about the base directory for running tests.
    // so try both options
    val propsFiles: List[Path] = projectNames.map { projectName =>
      val srcDir =
        Paths.get(s"../testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/").toAbsolutePath
      val f1 = srcDir.resolve(s"ScalaClean.properties")
      if (Files.exists(f1)) {
        f1
      } else {
        val srcDir =
          Paths.get(s"testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/").toAbsolutePath
        val f1 = srcDir.resolve(s"ScalaClean.properties")
        f1
      }

    }
    val options = cmdLine

    options.debug = runOptions.debug
    options.addComments = runOptions.addComments
    options.replace = runOptions.replace
    options._rulePlugins = runOptions.rulePluginText.asJava
    options.testOptions.validate = true
    options.testOptions.expectationSuffix = expectationSuffix
    options._paths = Some(propsFiles)

    customise(options)

    val projectSet = new ProjectSet(propsFiles: _*)

    val ruleToRun = rule(options, projectSet)
    val result = !ruleToRun.run() || options.replace
    if (!result)
      fail(s" Failed for projects $projectNames, options=$options")
  }

  val expectationSuffix: String

}
