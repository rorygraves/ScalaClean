package scalaclean.test

import scalaclean.model._

/**
 * A rule use to test the that overrides as set correctly
 */
class Test_internalDirectOverrides(model: ProjectModel) extends TestCommon("Test_internalDirectOverrides", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverrides.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalDirectOverrides(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}

/**
 * A rule use to test the that overrides as set correctly
 */
class Test_internalTransitiveOverrides(model: ProjectModel) extends TestCommon("Test_internalTransitiveOverrides", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverrides.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalTransitiveOverrides(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}

/**
 * A rule use to test the that overrides as set correctly
 */
class Test_allDirectOverrides(model: ProjectModel) extends TestCommon("Test_allDirectOverrides", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    val overrides = (modelElement.allDirectOverrides map (_._2.id)).sorted
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allDirectOverrides(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}

/**
 * A rule use to test the that overrides as set correctly
 */
class Test_allTransitiveOverrides(model: ProjectModel) extends TestCommon("Test_allTransitiveOverrides", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    val overrides = (modelElement.allTransitiveOverrides map (_._2.debugValue)).sorted
    overrides match {
      case Nil => ""
      case refs => refs.mkString(s"allTransitiveOverrides(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}

/**
 * A rule use to test the that overrides as set correctly
 */
class Test_internalDirectOverriddenBy(model: ProjectModel) extends TestCommon("Test_internalDirectOverriddenBy", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    val overrides = modelElement.internalDirectOverriddenBy.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalDirectOverriddenBy(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}

/**
 * A rule use to test the that overrides as set correctly
 */
class Test_internalTransitiveOverriddenBy(model: ProjectModel) extends TestCommon("Test_internalTransitiveOverriddenBy", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    val overrides = modelElement.internalTransitiveOverriddenBy.sorted
    overrides match {
      case Nil => ""
      case refs => debugValues(refs).mkString(s"internalTransitiveOverriddenBy(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}
