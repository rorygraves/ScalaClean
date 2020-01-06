package scalafix.scalaclean.cli

import java.nio.charset.StandardCharsets

import scalafix.scalaclean.FixUtils
import scalafix.v1
import scalafix.v1.SyntacticDocument

import scala.meta.{AbsolutePath, RelativePath}
import scala.meta.inputs.Input
import scala.meta.internal.io.FileIO
import scala.meta.internal.symtab.SymbolTable

object DocHelper {
  def readSemanticDoc(
                       classLoader: ClassLoader,
                       symtab: SymbolTable,
                       absSourcePath: AbsolutePath,
                       buildBase: AbsolutePath,
                       targetFile: RelativePath
                     ): (v1.SyntacticDocument, v1.SemanticDocument) = {

    val input = Input.VirtualFile(targetFile.toString, FileIO.slurp(absSourcePath, StandardCharsets.UTF_8))
    val syntacticDocument = SyntacticDocument.fromInput(input)

    val semanticDBPath = absSourcePath.toRelative(buildBase)

    val semanticDocument = FixUtils.fromPath(
      syntacticDocument,
      semanticDBPath,
      classLoader,
      symtab)

    (syntacticDocument, semanticDocument)
  }

  def readSyntacticDoc(
                        absSourcePath: AbsolutePath,
                        targetFile: RelativePath
                      ): v1.SyntacticDocument = {

    val input = Input.VirtualFile(targetFile.toString, FileIO.slurp(absSourcePath, StandardCharsets.UTF_8))
    val syntacticDocument = SyntacticDocument.fromInput(input)

    syntacticDocument
  }

}
