package scalaclean.test

import java.nio.file.Path

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

  def elementInfo(modelElement: ModelElement): String

  def run(targetFile: Path): List[SCPatch] = {
    object visitor extends ScalaCleanTreePatcher(new PatchStats, () => ???) {
      override def debug: Boolean       = false
      override def addComments: Boolean = false

      private def visit(modelElement: ModelElement): Boolean = {
        toPatch(elementInfo(modelElement), modelElement)
        true
      }

      override protected def visitNotInSource(modelElement: ModelElement): Boolean = visit(modelElement)
      override protected def visitInSource(modelElement: ModelElement): Boolean    = visit(modelElement)

      def toPatch(str: String, modelElement: ModelElement): Boolean = {
        if (str.nonEmpty)
          collect(SCPatch(modelElement.rawEnd, modelElement.rawEnd, s"/* $str */"))
        true
      }
    }
    val absPath = targetFile.toAbsolutePath.toRealPath()

    val sModel = model
      .allOf[SourceModel]
      .find{ _.filename == absPath }
      .getOrElse(throw new IllegalStateException(s"Unable to find source model for $targetFile ($absPath)\n${model
        .allOf[SourceModel].map(_.filename).mkString("\n")}"))

    visitor.visit(sModel)
    visitor.result
  }

}
