package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) extends CommonWriter(file) {
  private def commonPrefix(model: ModelSymbol): List[Any] = {
    import model._
    List(
      ioToken,
      legacyCsvIDString,
      idWithDeDuplicationSuffix,
      tree.symbol.flags.toHexString,
      sourceFile,
      posStart,
      posEnd,
      posFocus,
      traversal
    )
  }

  def write(modelSymbol: ModelSymbol): Unit = {
    val fields = modelSymbol match {
      case x: ModelMethod => commonPrefix(x) ::: List(x.isAbstract, x.sourceName, x.isTyped)
      case x: ModelClass  => commonPrefix(x)
      case x: ModelTrait  => commonPrefix(x)
      case x: ModelObject => commonPrefix(x)
      case x: ModelFields => commonPrefix(x) ::: List(x.syntheticName, x.isLazy, x.fieldCount)
      case x: ModelVal =>
        commonPrefix(x) ::: List(x.isAbstract, x.sourceName, x.fields.fold("")(_.newCsvString), x.isLazy)
      case x: ModelVar    => commonPrefix(x) ::: List(x.isAbstract, x.sourceName, x.fields.fold("")(_.newCsvString))
      case x: ModelSource => commonPrefix(x)
    }
    val msg = fields.mkString(",")

    if (!writer.writeLine(msg))
      logger.scopeLog(s" -->[DUPLICATE_ELE] $msg")

    if (logger.debug)
      logger.scopeLog(s" -->[ELEMENT] $msg")

  }

}
