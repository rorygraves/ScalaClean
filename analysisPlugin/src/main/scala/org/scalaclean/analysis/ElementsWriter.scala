package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) {
  var logger: ScopeLogging = _

  val writer = new SortedStringWriter(file.toPath)

  def write(modelSymbol: ModelSymbol) = {
    val msg = modelSymbol match {
      case model: ModelMethod =>
        s"${IoTokens.typeMethod},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd},${model.isAbstract},${model.sourceName},${model.isAbstract}"

      case model: ModelClass =>
        s"${IoTokens.typeClass},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}"

      case model: ModelTrait =>
        s"${IoTokens.typeTrait},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}"

      case model: ModelObject =>
        s"${IoTokens.typeObject},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}"

      case model: ModelVal =>
        s"${IoTokens.typeVal},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd},${model.isAbstract},${model.sourceName},${model.isLazy}"

      case model: ModelVar =>
        s"${IoTokens.typeVar},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd},${model.isAbstract},${model.sourceName}"

      case model: ModelSource =>
        s"${IoTokens.typeSource},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}"

    }
    if (! writer.writeLine(msg))
      logger.scopeLog(s" -->[DUPLICATE] $msg")

    if (logger.debug)
      logger.scopeLog(s" -->[ELEMENT] $msg")

  }
  def endUnit(): Unit = {
    writer.flush()
  }
  def finish(): Unit = {
    writer.close()
  }

}
