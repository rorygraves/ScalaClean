package org.scalaclean.analysis

import org.scalaclean.analysis.plugin.{ExtensionPlugin, ExtensionPluginFactory, PrivatiserPlugin}

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class ScalaCleanCompilerPlugin(override val global: Global) extends Plugin {

  override val name: String = "scalaclean-analysis-plugin"
  override val description: String = "ScalaClean analysis plugin"

  val component = new ScalaCompilerPluginComponent(global)

  //hardcoded for the moment
  component.extensions += PrivatiserPlugin.create(component,"")

  override def processOptions(
    options: List[String],
    error: String => Unit): Unit = {

    import scala.reflect.runtime.universe
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)

    val realOptions = options.distinct
    component.options = realOptions
    for (option <- realOptions) {
      if(option == "debug:true") {
        component.debug = true
      } else if(option.startsWith("extension:")) {
        val end = {
          val end = option.indexOf(':', 10)
          if ( end == -1) option.length else end
        }
        val fqn = option.substring(10, end)
        val module = runtimeMirror.staticModule(fqn)
        runtimeMirror.reflectModule(module).instance match {
          case valid: ExtensionPluginFactory => component.extensions += valid.create(component, option.substring(end))
          case null => throw new IllegalArgumentException("not a valid Extension FQN - expected the name of an object")
          case invalid => throw new IllegalArgumentException(s"not a valid Extension FQN - ${invalid.getClass.getName()} is not a ${classOf[ExtensionDescriptor[_]].getName}")
        }
      } else if(option.startsWith("srcdirs:")) {
        component.sourceDirs = option.substring(8).split(java.io.File.pathSeparatorChar).toList
      } else
        error(s"Option not recognised: $option")
    }
  }

  override val optionsHelp: Option[String] = Some(//
    s"""-P:$name:debug:true        Set debugging on the ScalaClean analysis plugin
       |-P:$name:srcdirs           The path of sources, seperated by ${java.io.File.pathSeparatorChar}
       |-P:$name:extension:<fqn>   Add an extension dataset. FQN is the fully qualified name of the appropriate ExtensionDescriptor object
       |""".stripMargin)

  override val components: List[PluginComponent] = List(component)
}

