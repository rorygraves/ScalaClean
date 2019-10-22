package scalaclean.test

import scalaclean.model._

/**
  * Basic node types test
  */
class TestNodes(model: ProjectModel) extends TestCommon("TestNodes", model) {
  def visit(modelElement: ModelElement): String = {
    modelElement.toString
  }
}
