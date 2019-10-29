package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) {
  var logger: ScopeLogging = _

  val writer = new SortedStringWriter(file.toPath)
  def commonPrefix(model: ModelSymbol): String =
    s"${model.ioToken},${model.csvString},${model.newCsvString},${model.tree.symbol.flags.toHexString},${model.sourceFile},${model.posStart},${model.posEnd},${model.traversal}"


  def write(modelSymbol: ModelSymbol) = {
    val msg = modelSymbol match {
      case model: ModelMethod =>
        s"${commonPrefix(model)},${model.isAbstract},${model.sourceName},${model.isAbstract}"

      case model: ModelClass =>
        commonPrefix(model)

      case model: ModelTrait =>
        commonPrefix(model)

      case model: ModelObject =>
        commonPrefix(model)

      case model: ModelVal =>
        s"${commonPrefix(model)},${model.isAbstract},${model.sourceName},${model.isLazy}"

      case model: ModelVar =>
        s"${commonPrefix(model)},${model.isAbstract},${model.sourceName}"

      case model: ModelSource =>
        s"${commonPrefix(model)}"

    }
    if (! writer.writeLine(msg))
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
