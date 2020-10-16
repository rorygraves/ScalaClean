package scalaclean

import org.scalatest.{ BeforeAndAfterAllConfigMap, ConfigMap }
import org.scalatest.funsuite.AnyFunSuite
import scalaclean.cli.SimpleRunOptions

abstract class AbstractProjectTests extends AnyFunSuite with BeforeAndAfterAllConfigMap {
  private var _overwrite  = false
  protected def overwrite = _overwrite

  override protected def beforeAll(configMap: ConfigMap): Unit = {
    _overwrite = configMap.getWithDefault("overwrite", "false").equalsIgnoreCase("true")
  }

  def projectTest(
                   projectName: String,
                   options: SimpleRunOptions = SimpleRunOptions()
                 ): Unit = {
    projectsTest(List(projectName), options)
  }

  def projectsTest(projectNames: List[String], options: SimpleRunOptions = SimpleRunOptions()): Unit

}
