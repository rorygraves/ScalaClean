package scalaclean

import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import org.scalatest.funsuite.AnyFunSuite
import scalaclean.cli.{DeadCodeProjectTestRunner, FinaliserProjectTestRunner, PrivatiserProjectTestRunner}

abstract class AbstractProjectTests extends AnyFunSuite with BeforeAndAfterAllConfigMap {
  private var overwrite = false

  override protected def beforeAll(configMap: ConfigMap) = {
    overwrite = configMap.getWithDefault("overwrite", "false").equalsIgnoreCase("true")
  }

  def deadCodeProjectTest(projectName: String, overwriteTarget: Boolean = false): Unit = {
    deadCodeProjectTest(List(projectName), overwriteTarget)
  }

  def deadCodeProjectTest(projectNames: List[String], overwriteTarget: Boolean): Unit = {
    val res = new DeadCodeProjectTestRunner(projectNames, overwrite || overwriteTarget).run()
    if(!res)
      fail(s" Failed for projects $projectNames, overwriteTarget=$overwriteTarget")
  }

  def privatiserProjectTest(projectName: String, overwriteTarget: Boolean = false): Unit = {
    privatiserProjectTest(List(projectName), overwriteTarget)
  }

  def privatiserProjectTest(projectNames: List[String], overwriteTarget: Boolean): Unit = {
    val res = new PrivatiserProjectTestRunner(projectNames, overwrite || overwriteTarget).run()
    if(!res)
      fail(s" Failed for projects $projectNames, overwriteTarget=$overwriteTarget")
  }


  def finaliserProjectTest(projectName: String, overwriteTarget: Boolean = false): Unit = {
    finaliserProjectTest(List(projectName), overwriteTarget)
  }

  def finaliserProjectTest(projectNames: List[String], overwriteTarget: Boolean): Unit = {
    val res = new FinaliserProjectTestRunner(projectNames, overwrite || overwriteTarget).run()
    if(!res)
      fail(s" Failed for projects $projectNames, overwriteTarget=$overwriteTarget")
  }


}
