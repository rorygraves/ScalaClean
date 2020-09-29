package scalaclean.util

import java.nio.charset.StandardCharsets

import scalafix.v1.SyntacticDocument

import scala.meta.inputs.Input
import scala.meta.internal.io.FileIO
import scala.meta.{ AbsolutePath, RelativePath }

object DocHelper {

  def readSyntacticDoc(
      absSourcePath: AbsolutePath,
      targetFile: RelativePath
  ): SyntacticDocument = {

    val input             = Input.VirtualFile(targetFile.toString, FileIO.slurp(absSourcePath, StandardCharsets.UTF_8))
    val syntacticDocument = SyntacticDocument.fromInput(input)

    syntacticDocument
  }

}
