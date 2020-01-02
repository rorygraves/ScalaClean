package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) {
  var logger: ScopeLogging = _

  val writer = new SortedStringWriter(file.toPath)

  def commonPrefix(model: ModelSymbol): String =
    s"${model.ioToken},${model.csvString},${model.newCsvString},${model.tree.symbol.flags.toHexString},${model.sourceFile},${model.posStart},${model.posEnd},${model.posFocus},${model.traversal}"


  def write(modelSymbol: ModelSymbol): Unit = {
    val msg = modelSymbol match {
      case model: ModelMethod =>
        s"${commonPrefix(model)},${model.isAbstract},${model.sourceName},${model.isTyped}"

      case model: ModelClass =>
        commonPrefix(model)

      case model: ModelTrait =>
        commonPrefix(model)

      case model: ModelObject =>
        commonPrefix(model)

      case model: ModelFields =>
        s"${commonPrefix(model)},${model.syntheticName},${model.isLazy},${model.fieldCount}"

      case model: ModelVal =>
        s"${commonPrefix(model)},${model.isAbstract},${model.sourceName},${model.fields.map(_.newCsvString).getOrElse("")},${model.isLazy}"

      case model: ModelVar =>
        s"${commonPrefix(model)},${model.isAbstract},${model.sourceName},${model.fields.map(_.newCsvString).getOrElse("")}"

      case model: ModelSource =>
        s"${commonPrefix(model)}"

    }
    if (!writer.writeLine(msg))
      logger.scopeLog(s" -->[DUPLICATE_ELE] $msg")

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
