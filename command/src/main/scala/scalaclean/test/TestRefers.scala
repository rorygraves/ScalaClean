package scalaclean.test

import scalaclean.model._

/**
 * A rule use to test that internal incoming references are set correctly,
 * needs to be run after TestAnalysis
 */
class Test_internalIncomingReferences(model: AllProjectsModel)
    extends TestCommon("Test_internalIncomingReferences", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalIncomingReferences", modelElement,
      modelElement.referredToByElement())

}

/**
 * A rule use to test that internal outgoing references are set correctly,
 * needs to be run after TestAnalysis
 */
class Test_internalOutgoingReferences(model: AllProjectsModel)
    extends TestCommon("Test_internalOutgoingReferences", model) {

  override def visitInSource(modelElement: ModelElement): String = {
    elementsInTestFormat("internalOutgoingReferences", modelElement,
      modelElement.refersToElement())
  }

}

/**
 * A rule use to test that internal and external outgoing references ar set correctly,
 * needs to be run after TestAnalysis
 */
class Test_allOutgoingReferences(model: AllProjectsModel) extends TestCommon("Test_allOutgoingReferences", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementIdsTestFormat("allOutgoingReferences", modelElement,
      modelElement.refersToElementId())

}
