package org.scalaclean.analysis.plugin

import org.scalaclean.analysis.{ ExtensionData, ModelSymbol, ScalaCompilerPluginComponent }

import scala.tools.nsc.Global

trait ExtensionPluginFactory {
  def create(sc: ScalaCompilerPluginComponent, param: String): ExtensionPlugin
}

trait ExtensionPlugin {
  val sc: ScalaCompilerPluginComponent
  val g: Global = sc.global

  def debug: Boolean = sc.debug

  def extendedData(mSymbol: ModelSymbol, tree: Option[g.Tree], symbol: g.Symbol, enclosingModel: List[ModelSymbol]): List[ExtensionData]
}
