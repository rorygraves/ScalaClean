package scalaclean

import org.scalatest.FunSuite
import org.scalatest.{ BeforeAndAfterAllConfigMap, ConfigMap }
import scalaclean.cli.{DeadCodeProjectTestRunner, PrivatiserProjectTestRunner}

abstract class AbstractProjectTests extends FunSuite with BeforeAndAfterAllConfigMap {
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


}
