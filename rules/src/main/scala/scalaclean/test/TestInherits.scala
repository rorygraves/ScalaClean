package scalaclean.test

import scalaclean.model._

/**
  * A rule use to test the that incoming references ar set correctly,
  * needs to be run after TestAnalysis
  */
class TestInherits extends TestCommon("TestInherits") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.directOverrides
    val internalOverrides = overrides.flatMap(model.getSymbol[ModelElement](_))
    val names = internalOverrides.map(_.symbol.value).distinct.sorted
    names match {
      case Nil => ""
      case refs => refs.mkString(s"internalDirectOverrides(${modelElement.symbol.value}) - ", " :: ", "")
    }
  }
}
