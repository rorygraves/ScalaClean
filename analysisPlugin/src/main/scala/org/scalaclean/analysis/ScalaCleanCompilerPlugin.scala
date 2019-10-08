package org.scalaclean.analysis

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class ScalaCleanCompilerPlugin(override val global: Global) extends Plugin {

  override val name: String = "scalaclean-analysis-plugin"
  override val description: String = "ScalaClean analysis plugin"

  var debug = false

  override def processOptions(
    options: List[String],
    error: String => Unit): Unit = {
    for (option <- options) {
      if(option == "debug:true") {
        component.debug = true
      } else if(option.startsWith("srcdirs:")) {
        component.sourceDirs = option.substring(8).split(java.io.File.pathSeparatorChar).toList
      } else
        error(s"Option not recognised: $option")
    }
  }

  override val optionsHelp: Option[String] = Some(s"use -P:$name:debug:true        Set debugging on the ScalaClean analysis plugin")

  val component = new ScalaCompilerPluginComponent(global)
  override val components: List[PluginComponent] = List(component)
}

