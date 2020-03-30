package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) {
  var logger: ScopeLogging = _

  val writer = new SortedStringWriter(file.toPath)

  def commonPrefix(model: ModelSymbol): List[Any] = {
    import model._
    List(ioToken, csvString, newCsvString, tree.symbol.flags.toHexString, sourceFile, posStart, posEnd, traversal)
  }

  def write(modelSymbol: ModelSymbol): Unit = {
    val fields = modelSymbol match {
      case x: ModelMethod => commonPrefix(x) ::: List(x.isAbstract, x.sourceName, x.isTyped)
      case x: ModelClass  => commonPrefix(x)
      case x: ModelTrait  => commonPrefix(x)
      case x: ModelObject => commonPrefix(x)
      case x: ModelFields => commonPrefix(x) ::: List(x.syntheticName, x.isLazy, x.fieldCount)
      case x: ModelVal    => commonPrefix(x) ::: List(x.isAbstract, x.sourceName, x.fields.fold("")(_.newCsvString), x.isLazy)
      case x: ModelVar    => commonPrefix(x) ::: List(x.isAbstract, x.sourceName, x.fields.fold("")(_.newCsvString))
      case x: ModelSource => commonPrefix(x)
    }
    val msg = fields.mkString(",")

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
