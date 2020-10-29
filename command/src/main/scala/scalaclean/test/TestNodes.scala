package scalaclean.test

import scalaclean.model._

/** Basic node types test */
class TestNodes(model: AllProjectsModel) extends TestCommon("TestNodes", model) {

  override def visitInSource(modelElement: ModelElement): String = {
    modelElement.toString
  }

}
