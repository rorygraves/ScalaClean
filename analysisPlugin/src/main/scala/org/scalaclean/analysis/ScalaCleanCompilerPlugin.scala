package org.scalaclean.analysis

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class ScalaCleanCompilerPlugin(override val global: Global) extends Plugin {

  override val name: String = "scalaclean-compiler-plugin"
  override val description: String = "ScalaClean analysis plugin"

  override val components: List[PluginComponent] = List(new ScalaCompilerPluginComponent(global))
}

