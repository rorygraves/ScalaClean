package scalaclean.test

import scalaclean.model.Filters._
import scalaclean.model._

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extends(directOnly: Boolean, filter: ExtendsClassLike, testNameSuffix: String, model: ProjectModel)
    extends TestCommon(s"Test_extends$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      val ex = classLike.extendsClassLike(directOnly, filter).toList.sortBy(_._2.debugValue)
      val strings = ex.map { case (clOpt: Option[ClassLike], elementId: ElementId) =>
        clOpt.foreach(cl =>
          assert(cl.modelElementId == elementId, s"cl.modelElementId(${cl.modelElementId}) != elementId($elementId)")
        )
        s"{${clOpt.isDefined}}~${elementId.debugValue}"
      }
      strings match {
        case Nil  => ""
        case refs => refs.mkString(s"extends(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
      }
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
    model: ProjectModel
) extends TestCommon(s"Test_extendsCompiled$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      val ex      = classLike.extendsClassLikeCompiled(directOnly, filter).toList.sortBy(_.modelElementId.debugValue)
      val strings = ex.map(_.modelElementId.debugValue)
      strings match {
        case Nil  => ""
        case refs => refs.mkString(s"extendsCompiled(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
      }
    case _ => ""
  }

}

/**
 * A rule use to test that extends API works correctly,
 * needs to be run after TestAnalysis
 */
class Test_extendedBy(directOnly: Boolean, filter: ExtendedByClassLike, testNameSuffix: String, model: ProjectModel)
    extends TestCommon(s"Test_extendedBy$testNameSuffix", model) {

  override def visitInSource(modelElement: ModelElement): String = modelElement match {
    case classLike: ClassLike =>
      val ex      = classLike.extendedByClassLike(directOnly, filter).toList.sortBy(_.modelElementId.debugValue)
      val strings = ex.map(_.modelElementId.debugValue)
      strings match {
        case Nil  => ""
        case refs => refs.mkString(s"extends(${modelElement.modelElementId.debugValue}) - ", " :: ", "")
      }
    case _ => ""
  }

}
