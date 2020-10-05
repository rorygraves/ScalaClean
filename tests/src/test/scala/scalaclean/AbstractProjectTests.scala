package scalaclean

import org.scalatest.{ BeforeAndAfterAllConfigMap, ConfigMap }
import org.scalatest.funsuite.AnyFunSuite
import scalaclean.cli.{
  DeadCodeProjectTestRunner,
  FinaliserProjectTestRunner,
  PrivatiserProjectTestRunner,
  SimpleRunOptions
}

abstract class AbstractProjectTests extends AnyFunSuite with BeforeAndAfterAllConfigMap {
  private var _overwrite  = false
  protected def overwrite = _overwrite

  val expectationSuffix: String
  val taskName: String

  override protected def beforeAll(configMap: ConfigMap): Unit = {
    _overwrite = configMap.getWithDefault("overwrite", "false").equalsIgnoreCase("true")
  }

  def deadCodeProjectTest(
      projectName: String,
      options: SimpleRunOptions = SimpleRunOptions()
  ): Unit = {
    deadCodeProjectTests(List(projectName), options)
  }

  def deadCodeProjectTests(projectNames: List[String], options: SimpleRunOptions = SimpleRunOptions()): Unit = {
    val res =
      new DeadCodeProjectTestRunner(projectNames, taskName, expectationSuffix, options.orReplace(overwrite)).run()
    if (!res)
      fail(s" Failed for projects $projectNames, options=$options")
  }

  def privatiserProjectTest(
      projectName: String,
      options: SimpleRunOptions = SimpleRunOptions()
  ): Unit = {
    privatiserProjectTests(List(projectName), options)
  }

  def privatiserProjectTests(
      projectNames: List[String],
      options: SimpleRunOptions = SimpleRunOptions()
  ): Unit = {
    val res = new PrivatiserProjectTestRunner(projectNames, options.orReplace(overwrite)).run()
    if (!res)
      fail(s" Failed for projects $projectNames, options=$options")
  }

  def finaliserProjectTest(
      projectName: String,
      options: SimpleRunOptions = SimpleRunOptions()
  ): Unit = {
    finaliserProjectTests(List(projectName), options)
  }

  def finaliserProjectTests(
      projectNames: List[String],
      options: SimpleRunOptions = SimpleRunOptions()
  ): Unit = {
    val res = new FinaliserProjectTestRunner(projectNames, options.orReplace(overwrite)).run()
    if (!res)
      fail(s" Failed for projects $projectNames, options=$options")
  }

}
