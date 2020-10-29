package scalaclean.test

import scalaclean.model.Filters._
import scalaclean.model._

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extends(directOnly: Boolean, filter: ExtendsClassLike, testNameSuffix: String, model: AllProjectsModel)
    extends TestCommon(s"Test_extends$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      elementAndIdsInTestFormat("extends", modelElement,
        classLike.extendsClassLike(directOnly, filter))
    case _ => ""
  }

}

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extendsCompiled(
    directOnly: Boolean,
    filter: ExtendsClassLikeCompiled,
    testNameSuffix: String,
    model: AllProjectsModel
) extends TestCommon(s"Test_extendsCompiled$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      elementsInTestFormat("extendsCompiled", modelElement,
        classLike.extendsClassLikeCompiled(directOnly, filter))
    case _ => ""
  }

}

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extendedBy(directOnly: Boolean, filter: ExtendedByClassLike, testNameSuffix: String, model: AllProjectsModel)
    extends TestCommon(s"Test_extendedBy$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      elementsInTestFormat("extendedBy", modelElement,
        classLike.extendedByClassLike(directOnly, filter))
    case _ => ""
  }

}
