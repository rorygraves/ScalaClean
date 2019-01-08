package scalaclean.test

import scalaclean.model._
/**
  * A rule use to test that internal incoming references are set correctly,
  * needs to be run after TestAnalysis
  */
class Test_internalIncomingReferences extends TestCommon("Test_internalIncomingReferences") {
  def visit(modelElement: ModelElement): String = {
    modelElement.internalIncomingReferences.map(_._1.symbol.value).distinct.sorted match  {
      case Nil => ""
      case refs => refs.mkString(s"internalIncomingReferences(${modelElement.symbol.value}) - "," :: ", "")
    }
  }
}

/**
  * A rule use to test that internal outgoing references are set correctly,
  * needs to be run after TestAnalysis
  */
class Test_internalOutgoingReferences extends TestCommon("Test_internalOutgoingReferences") {
  def visit(modelElement: ModelElement): String = {
    modelElement.internalOutgoingReferences.map(_._1.symbol.value).distinct.sorted match  {
      case Nil => ""
      case refs => refs.mkString(s"internalOutgoingReferences(${modelElement.symbol.value}) - "," :: ", "")
    }
  }
}

/**
  * A rule use to test that internal and external outgoing references ar set correctly,
  * needs to be run after TestAnalysis
  */
class Test_allOutgoingReferences extends TestCommon("Test_allOutgoingReferences") {
  def visit(modelElement: ModelElement): String = {
    modelElement.allOutgoingReferences.map(_._3.value).distinct.sorted match  {
      case Nil => ""
      case refs => refs.mkString(s"allOutgoingReferences(${modelElement.symbol.value}) - "," :: ", "")
    }
  }
}
