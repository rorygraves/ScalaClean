//package org.scalaclean.analysis.plugin
//
//import org.scalaclean.analysis.{ExtensionData, ModelSymbol, ScalaCompilerPluginComponent}
//
//object JunitPlugin extends ExtensionPluginFactory {
//  override def create(sc: ScalaCompilerPluginComponent, paramIgnored: String) = new JunitPlugin(sc)
//}
//
//class JunitPlugin(val sc: ScalaCompilerPluginComponent) extends ExtensionPlugin {
//  override def extendedData(mSymbol: ModelSymbol, tree: g.Tree): List[ExtensionData] = {
//    tree.symbol.annotations collect {
//      case annotation if annotation.atp.baseClasses.head.nameString.startsWith("org.junit")  =>
//        AnnotationDataBuilder.buildSimpleAnnotation(g)(tree, annotation)
//    }
//  }
//}
