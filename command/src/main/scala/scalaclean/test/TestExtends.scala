package scalaclean.test

import scalaclean.model._

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extends(
                    directOnly: Option[Boolean],
                    filter: Option[ExtendsReference => Boolean],
                    testNameSuffix: String,
                    model: AllProjectsModel)
    extends TestCommon(s"Test_extends$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      elementAndIdsInTestFormat("extends", modelElement,
        classLike.extendsFull(directOnly, filter))
    case _ => ""
  }

}

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extendsCompiled(
    direct: Option[Boolean],
    filter: Option[ExtendsInternalReference => Boolean],
    testNameSuffix: String,
    model: AllProjectsModel
) extends TestCommon(s"Test_extendsCompiled$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      elementsInTestFormat("extendsCompiled", modelElement,
        classLike.extendsElement(direct, filter))
    case _ => ""
  }

}

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extendedBy(
                       directOnly: Option[Boolean],
                       filter: Option[ExtendedByReference => Boolean],
                       testNameSuffix: String,
                       model: AllProjectsModel)
    extends TestCommon(s"Test_extendedBy$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      elementsInTestFormat("extendedBy", modelElement,
        classLike.extendedByElement(directOnly, filter))
    case _ => ""
  }

}
