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
    sourceDirectory: AbsolutePath,
    sourceRoot: AbsolutePath,
    targetFile: RelativePath
  ): v1.SemanticDocument = {

    //    val sourceDirectory = inputSourceDirectories.head
    val inputPath = sourceDirectory.resolve(targetFile)

    val semanticdbPath = inputPath.toRelative(sourceRoot)

    val input = Input.VirtualFile(targetFile.toString, FileIO.slurp(inputPath, StandardCharsets.UTF_8))
    val doc = SyntacticDocument.fromInput(input)

    FixUtils.fromPath(
      doc,
      semanticdbPath,
      classLoader,
      symtab)

  }
}
