package scalaclean.test

import scalaclean.model._
import scalaclean.util._

/**
  * A rule use to test the that incoming references ar set correctly,
  * needs to be run after ScalaCleanTestAnalysis
  */
abstract class TestBase(val name: String, model: ProjectModel) {

  def beforeStart(): Unit = {
    println(s"Test Rule $name beforeStart START")
    println(s"Test Rule $name size ${model.allOf[ModelElement].size}")
    println(s"Cleaner Rule $name beforeStart END")
  }

  def visitElement(modelElement: ModelElement): String

  def run(targetFile: String): List[SCPatch] = {
    val visitor: ElementTreeVisitor = new ElementTreeVisitor {
      override protected def visitElement(modelElement: ModelElement): Boolean = {
        toPatch(visitElement(modelElement), modelElement)
        true
      }

      def toPatch(str: String, modelElement: ModelElement): Boolean = {
        if (str.nonEmpty)
          collect(SCPatch(modelElement.rawEnd,modelElement.rawEnd,s"/* $str */"))
        true
      }
    }

    val targetFileName = targetFile.toString
    val sModel = model.allOf[SourceModel].filter(_.toString.contains(targetFileName)).toList.headOption.getOrElse(throw new IllegalStateException(s"Unable to find source model for $targetFileName"))

    visitor.visit(sModel)
    visitor.result
  }

}
