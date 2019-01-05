package scalaclean.test

import scalaclean.model._
/**
  * A rule use to test the that incoming references ar set correctly,
  * needs to be run after TestAnalysis
  */
class TestRefers extends TestCommon("TestRefers") {
  def visit(modelElement: ModelElement): String = {
    modelElement.internalIncomingReferences.map(_._1.symbol.value).distinct.sorted match  {
      case Nil => ""
      case refs => refs.mkString(s"internalIncomingReferences(${modelElement.symbol.value}) - "," :: ", "")
    }
  }
}
