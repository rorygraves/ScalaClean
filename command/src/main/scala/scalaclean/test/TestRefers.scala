package scalaclean.test

import scalaclean.model._

/**
 * A rule use to test that internal incoming references are set correctly,
 * needs to be run after TestAnalysis
 */
class Test_internalIncomingReferences(model: ProjectModel)
    extends TestCommon("Test_internalIncomingReferences", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalIncomingReferences", modelElement,
      modelElement.internalIncomingReferences.map(_._1))

}

/**
 * A rule use to test that internal outgoing references are set correctly,
 * needs to be run after TestAnalysis
 */
class Test_internalOutgoingReferences(model: ProjectModel)
    extends TestCommon("Test_internalOutgoingReferences", model) {

  override def visitInSource(modelElement: ModelElement): String = {
    elementsInTestFormat("internalOutgoingReferences", modelElement,
      modelElement.internalOutgoingReferences.map(_._1))
  }

}

/**
 * A rule use to test that internal and external outgoing references ar set correctly,
 * needs to be run after TestAnalysis
 */
class Test_allOutgoingReferences(model: ProjectModel) extends TestCommon("Test_allOutgoingReferences", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementIdsTestFormat("allOutgoingReferences", modelElement,
      modelElement.allOutgoingReferences.map(_._2.toElementId))

}
