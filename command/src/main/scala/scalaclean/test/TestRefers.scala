package scalaclean.test

import scalaclean.model._

/**
 * A rule use to test that internal incoming references are set correctly,
 * needs to be run after TestAnalysis
 */
class Test_internalIncomingReferences(model: ProjectModel) extends TestCommon("Test_internalIncomingReferences", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    modelElement.internalIncomingReferences.map(_._1.modelElementId.debugValue).distinct.sorted match {
      case Nil => ""
      case refs => refs.mkString(s"internalIncomingReferences(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}

/**
 * A rule use to test that internal outgoing references are set correctly,
 * needs to be run after TestAnalysis
 */
class Test_internalOutgoingReferences(
                                       model: ProjectModel) extends TestCommon("Test_internalOutgoingReferences", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    modelElement.internalOutgoingReferences.map(_._1.modelElementId.debugValue).distinct.sorted match {
      case Nil => ""
      case refs => refs.mkString(s"internalOutgoingReferences(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}

/**
 * A rule use to test that internal and external outgoing references ar set correctly,
 * needs to be run after TestAnalysis
 */
class Test_allOutgoingReferences(model: ProjectModel) extends TestCommon("Test_allOutgoingReferences", model) {
  override def visitInSource(modelElement: ModelElement): String = {
    modelElement.allOutgoingReferences.map(_._2.toElementId.id).distinct.sorted match {
      case Nil => ""
      case refs => refs.mkString(s"allOutgoingReferences(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
}
