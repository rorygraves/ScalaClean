package scalaclean.test

import scalaclean.model._

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalDirectOverrides extends TestCommon("Test_internalDirectOverrides") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverrides.sorted
    overrides match {
      case Nil => ""
      case refs => justSymbols(refs).mkString(s"internalDirectOverrides(${modelElement.symbol.debugValue}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalTransitiveOverrides extends TestCommon("Test_internalTransitiveOverrides") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverrides.sorted
    overrides match {
      case Nil => ""
      case refs => justSymbols(refs).mkString(s"internalTransitiveOverrides(${modelElement.symbol.debugValue}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_allDirectOverrides extends TestCommon("Test_allDirectOverrides") {
  def visit(modelElement: ModelElement): String = {
    println("HERE " + modelElement.symbol)
    val overrides = (modelElement.allDirectOverrides map (_._2.debugValue)).sorted
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allDirectOverrides(${modelElement.symbol.debugValue}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_allTransitiveOverrides extends TestCommon("Test_allTransitiveOverrides") {
  def visit(modelElement: ModelElement): String = {
    val overrides = (modelElement.allTransitiveOverrides map (_._2.debugValue)).sorted
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allTransitiveOverrides(${modelElement.symbol.debugValue}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalDirectOverriddenBy extends TestCommon("Test_internalDirectOverriddenBy") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverriddenBy.sorted
    overrides match {
      case Nil => ""
      case refs => justSymbols(refs).mkString(s"internalDirectOverriddenBy(${modelElement.symbol.debugValue}) - ", " :: ", "")
    }
  }
}
/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalTransitiveOverriddenBy extends TestCommon("Test_internalTransitiveOverriddenBy") {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverriddenBy.sorted
    overrides match {
      case Nil => ""
      case refs => justSymbols(refs).mkString(s"internalTransitiveOverriddenBy(${modelElement.symbol.debugValue}) - ", " :: ", "")
    }
  }
}
