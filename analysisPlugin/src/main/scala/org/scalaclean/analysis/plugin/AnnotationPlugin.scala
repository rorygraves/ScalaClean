package org.scalaclean.analysis.plugin

import org.scalaclean.analysis.{ExtensionData, ModelSymbol, ScalaCompilerPluginComponent}

import scala.collection.mutable


object AnnotationPlugin extends ExtensionPluginFactory {
  override def create(
    sc: ScalaCompilerPluginComponent,
    param: String): ExtensionPlugin = new AnnotationPlugin(sc)
}

class AnnotationPlugin(val sc: ScalaCompilerPluginComponent) extends ExtensionPlugin {
  override def extendedData(
    mSymbol: ModelSymbol,
    tree: g.Tree,
    enclosingModel: List[ModelSymbol]): List[ExtensionData] = {
    val extensionData = mutable.ListBuffer[ExtensionData]()

    tree.symbol.annotations.foreach(annotation =>
      extensionData += AnnotationDataBuilder.buildSimpleAnnotation(g)(tree, annotation)
    )
    extensionData.result
  }
}