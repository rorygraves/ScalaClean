package scalaclean.test

import scalaclean.model._

/**
  * Basic node types test
  */
class TestNodes extends TestCommon("TestNodes") {
  def visit(modelElement: ModelElement): String = {
    modelElement.toString
  }
}
