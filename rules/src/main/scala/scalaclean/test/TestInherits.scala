package scalaclean.test

import scalaclean.model._

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalDirectOverrides extends TestCommon("Test_internalDirectOverrides") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverrides
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"internalDirectOverrides(${modelElement.symbol.value}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalTransitiveOverrides extends TestCommon("Test_internalTransitiveOverrides") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverrides
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"internalTransitiveOverrides(${modelElement.symbol.value}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_allDirectOverrides extends TestCommon("Test_allDirectOverrides") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.allDirectOverrides map (_._2)
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allDirectOverrides(${modelElement.symbol.value}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_allTransitiveOverrides extends TestCommon("Test_allTransitiveOverrides") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.allTransitiveOverrides map (_._2)
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allTransitiveOverrides(${modelElement.symbol.value}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalDirectOverriddenBy extends TestCommon("Test_internalDirectOverriddenBy") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverriddenBy
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"internalDirectOverriddenBy(${modelElement.symbol.value}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalTransitiveOverriddenBy extends TestCommon("Test_internalTransitiveOverriddenBy") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverrides
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"internalTransitiveOverriddenBy(${modelElement.symbol.value}) - ", " :: ", "")
    }
  }
}
