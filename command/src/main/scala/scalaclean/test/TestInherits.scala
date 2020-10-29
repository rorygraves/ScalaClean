package scalaclean.test

import scalaclean.model._

/** A rule use to test the that overrides as set correctly */
class Test_internalDirectOverrides(model: AllProjectsModel) extends TestCommon("Test_internalDirectOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalDirectOverrides", modelElement,
      modelElement.internalDirectOverrides)

}

/** A rule use to test the that overrides as set correctly */
class Test_internalTransitiveOverrides(model: AllProjectsModel)
    extends TestCommon("Test_internalTransitiveOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalTransitiveOverrides", modelElement,
      modelElement.internalTransitiveOverrides)
}

/** A rule use to test the that overrides as set correctly */
class Test_allDirectOverrides(model: AllProjectsModel) extends TestCommon("Test_allDirectOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementAndIdsInTestFormat("allDirectOverrides", modelElement,
      modelElement.allDirectOverrides.iterator)
}

/** A rule use to test the that overrides as set correctly */
class Test_allTransitiveOverrides(model: AllProjectsModel) extends TestCommon("Test_allTransitiveOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementAndIdsInTestFormat("allTransitiveOverrides", modelElement,
      modelElement.allTransitiveOverrides.iterator)
}

/** A rule use to test the that overrides as set correctly */
class Test_internalDirectOverriddenBy(model: AllProjectsModel)
    extends TestCommon("Test_internalDirectOverriddenBy", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalDirectOverriddenBy", modelElement,
      modelElement.internalDirectOverriddenBy)

}

/** A rule use to test the that overrides as set correctly */
class Test_internalTransitiveOverriddenBy(model: AllProjectsModel)
    extends TestCommon("Test_internalTransitiveOverriddenBy", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalTransitiveOverriddenBy", modelElement,
      modelElement.internalTransitiveOverriddenBy)

}
