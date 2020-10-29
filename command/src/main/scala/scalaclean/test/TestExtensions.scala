package scalaclean.test

import scalaclean.model._

/**
 * A rule use to test that annotations are st correctly,
 * needs to be run after TestAnalysis
 */
class TestExtensions(model: AllProjectsModel) extends TestCommon("TestExtensions", model) {

  override def visitInSource(modelElement: ModelElement): String = {
    modelElement.annotations match {
      case Nil => "XX"
      case ann =>
        val asDebug = (ann.map(_.toString)).toList.sorted
        asDebug.mkString(s"annotations(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }

}
