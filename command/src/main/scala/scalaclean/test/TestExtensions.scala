package scalaclean.test

import scalaclean.model._

/**
  * A rule use to test that annotations are st correctly,
  * needs to be run after TestAnalysis
  */
class Test_annotations(
                        model: ProjectModel) extends TestCommon("Test_annotations", model) {
  def visit(modelElement: ModelElement): String = {
    modelElement.annotations match {
      case Nil => "XX"
      case ann =>
        val asDebug = ann.map(_.toString).toList.sorted
        asDebug.mkString(s"annotations(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}