package scalaclean.rules.plugin

import scalaclean.rules.RuleRun

object RulePluginFactory {
  def apply(objectNameAndParams: String): RulePlugin = {
    val (objectName, params) = {
      objectNameAndParams.split(":", 2) match {
        case Array(objectName, params) => (objectName, params)
        case Array(objectName) => (objectName, "")
      }
    }
    apply(objectName, params)
  }

  def apply(objectName: String, params: String): RulePlugin = {
    import scala.reflect.runtime.universe

    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
    val module = runtimeMirror.staticModule(objectName)
    val obj = runtimeMirror.reflectModule(module)
    val rulePluginFactory = obj.instance.asInstanceOf[RulePluginFactory]
    rulePluginFactory.build(params)
  }
}
trait RulePluginFactory {
  def build(params:String): RulePlugin
}
trait RulePlugin {
  //used for debug
  def name: String

  def beforeMainRule(mainRule: RuleRun[_]): Unit
  def afterMainRule(mainRule: RuleRun[_]): Unit

}
