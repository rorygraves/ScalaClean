package scalaclean.test

import scalaclean.model._

/** A rule use to test the that overrides as set correctly */
class Test_internalDirectOverrides(model: AllProjectsModel) extends TestCommon("Test_internalDirectOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalDirectOverrides", modelElement,
      modelElement.overridesElement(direct = Some(true)))

}

/** A rule use to test the that overrides as set correctly */
class Test_internalTransitiveOverrides(model: AllProjectsModel)
    extends TestCommon("Test_internalTransitiveOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalTransitiveOverrides", modelElement,
      modelElement.overridesElement())
}

/** A rule use to test the that overrides as set correctly */
class Test_allDirectOverrides(model: AllProjectsModel) extends TestCommon("Test_allDirectOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementAndIdsInTestFormat("allDirectOverrides", modelElement,
      modelElement.overridesFull(direct=Some(true)))
}

/** A rule use to test the that overrides as set correctly */
class Test_allTransitiveOverrides(model: AllProjectsModel) extends TestCommon("Test_allTransitiveOverrides", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementAndIdsInTestFormat("allTransitiveOverrides", modelElement,
      modelElement.overridesFull())
}

/** A rule use to test the that overrides as set correctly */
class Test_internalDirectOverriddenBy(model: AllProjectsModel)
    extends TestCommon("Test_internalDirectOverriddenBy", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalDirectOverriddenBy", modelElement,
      modelElement.overriddenByElement(direct = Some(true)))

}

/** A rule use to test the that overrides as set correctly */
class Test_internalTransitiveOverriddenBy(model: AllProjectsModel)
    extends TestCommon("Test_internalTransitiveOverriddenBy", model) {

  override def visitInSource(modelElement: ModelElement): String =
    elementsInTestFormat("internalTransitiveOverriddenBy", modelElement,
      modelElement.overriddenByElement())

}
