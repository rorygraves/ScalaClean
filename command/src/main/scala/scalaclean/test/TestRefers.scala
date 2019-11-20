package scalaclean.test

import scalaclean.model._

/**
  * A rule use to test that internal incoming references are set correctly,
  * needs to be run after TestAnalysis
  */
class Test_internalIncomingReferences(model: ProjectModel) extends TestCommon("Test_internalIncomingReferences", model) {
  def visit(modelElement: ModelElement): String = {
    modelElement.internalIncomingReferences.map(_._1.legacySymbol.debugValue).distinct.sorted match {
      case Nil => ""
      case refs => refs.mkString(s"internalIncomingReferences(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}

/**
  * A rule use to test that internal outgoing references are set correctly,
  * needs to be run after TestAnalysis
  */
class Test_internalOutgoingReferences(
                                       model: ProjectModel) extends TestCommon("Test_internalOutgoingReferences", model) {
  def visit(modelElement: ModelElement): String = {
    modelElement.internalOutgoingReferences.map(_._1.legacySymbol.debugValue).distinct.sorted match {
      case Nil => ""
      case refs => refs.mkString(s"internalOutgoingReferences(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}

/**
  * A rule use to test that internal and external outgoing references ar set correctly,
  * needs to be run after TestAnalysis
  */
class Test_allOutgoingReferences(model: ProjectModel) extends TestCommon("Test_allOutgoingReferences", model) {
  def visit(modelElement: ModelElement): String = {
    modelElement.allOutgoingReferences.map(_._2.toNewElementId.id).distinct.sorted match {
      case Nil => ""
      case refs => refs.mkString(s"allOutgoingReferences(${modelElement.legacySymbol.debugValue}) - ", " :: ", "")
    }
  }
}
