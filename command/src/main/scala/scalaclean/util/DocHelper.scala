package scalaclean.util

import scalaclean.model.SourceModel
import scalafix.internal.config.ScalaVersion
import scalafix.v1.SyntacticDocument

import scala.meta.inputs.Input

object DocHelper {

  def readSyntacticDoc(model: SourceModel, content: String): SyntacticDocument = {
    val input = Input.VirtualFile(model.filename.toString, content)
    SyntacticDocument.fromInput(input, ScalaVersion.scala2)
  }
}
