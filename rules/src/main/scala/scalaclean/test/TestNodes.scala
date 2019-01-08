package scalaclean.test

import scalaclean.model._

/**
  * Basic node types test
  */
class Test_nodes extends TestCommon("Test_nodes") {
  def visit(modelElement: ModelElement): String = {
    modelElement.toString
  }
}
