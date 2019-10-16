package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) {

  val writer = new SortedStringWriter(file.toPath)

  def write(modelSymbol: ModelSymbol) = modelSymbol match {
    case model: ModelMethod =>
      writer.writeLine(s"${IoTokens.typeMethod},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd},${model.isAbstract},${model.sourceName},${model.isAbstract}")
    case model: ModelClass =>
      writer.writeLine(s"${IoTokens.typeClass},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}")
    case model: ModelTrait =>
      writer.writeLine(s"${IoTokens.typeTrait},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}")
    case model: ModelObject =>
      writer.writeLine(s"${IoTokens.typeObject},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}")
    case model: ModelVal =>
      writer.writeLine(s"${IoTokens.typeVal},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd},${model.isAbstract},${model.sourceName},${model.isLazy}")
    case model: ModelVar =>
      writer.writeLine(s"${IoTokens.typeVar},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd},${model.isAbstract},${model.sourceName}")
    case model: ModelSource =>
      writer.writeLine(s"${IoTokens.typeSource},${model.csvString},${model.sourceFile},${model.posStart},${model.posEnd}")

  }
  def finish(): Unit = {
    writer.close()
  }

}
