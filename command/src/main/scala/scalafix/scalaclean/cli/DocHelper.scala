package scalafix.scalaclean.cli

import java.nio.charset.StandardCharsets

import scalafix.v1
import scalafix.v1.SyntacticDocument

import scala.meta.inputs.Input
import scala.meta.internal.io.FileIO
import scala.meta.{AbsolutePath, RelativePath}

object DocHelper {
  def readSyntacticDoc(
                        absSourcePath: AbsolutePath,
                        targetFile: RelativePath
                      ): v1.SyntacticDocument = {

    val input = Input.VirtualFile(targetFile.toString, FileIO.slurp(absSourcePath, StandardCharsets.UTF_8))
    val syntacticDocument = SyntacticDocument.fromInput(input)

    syntacticDocument
  }

}
