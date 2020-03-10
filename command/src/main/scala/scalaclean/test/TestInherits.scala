package scalaclean.test

import scalaclean.model._

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalDirectOverrides(model: ProjectModel) extends TestCommon("Test_internalDirectOverrides", model) {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverrides.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalDirectOverrides(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalTransitiveOverrides(model: ProjectModel) extends TestCommon("Test_internalTransitiveOverrides", model) {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverrides.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalTransitiveOverrides(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_allDirectOverrides(model: ProjectModel) extends TestCommon("Test_allDirectOverrides", model) {
  def visit(modelElement: ModelElement): String = {
    val overrides = (modelElement.allDirectOverrides map (_._2.id)).sorted
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allDirectOverrides(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_allTransitiveOverrides(model: ProjectModel) extends TestCommon("Test_allTransitiveOverrides", model) {
  def visit(modelElement: ModelElement): String = {
    val overrides = (modelElement.allTransitiveOverrides map (_._2.debugValue)).sorted
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allTransitiveOverrides(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalDirectOverriddenBy(model: ProjectModel) extends TestCommon("Test_internalDirectOverriddenBy", model) {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverriddenBy.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalDirectOverriddenBy(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}

/**
  * A rule use to test the that overrides as set correctly
  */
class Test_internalTransitiveOverriddenBy(model: ProjectModel) extends TestCommon("Test_internalTransitiveOverriddenBy", model) {
  def visit(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverriddenBy.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalTransitiveOverriddenBy(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}
