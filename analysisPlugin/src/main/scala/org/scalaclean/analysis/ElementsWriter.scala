package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) extends CommonWriter(file) {
  private def commonPrefix(model: ModelSymbol): List[Any] = {
    import model._
    List(
      ioToken,
      idWithDeDuplicationSuffix,
      symbol.flags.toHexString,
      sourceFile,
      posStart,
      posEnd,
      posFocus,
      traversal
    )
  }


  def write(modelSymbol: ModelSymbol): Unit = {
    def fieldCommon(x: ModelField) = {
      List(
        x.isAbstract, //
        x.sourceName, //
        x.fields.fold("")(_.newCsvString) //
      )
    }

    val fields = modelSymbol match {
      case x: ModelMethod => commonPrefix(x) ::: List(x.isAbstract, x.sourceName, x.isTyped, x.isScalaCleanSynthetic)
      case x: ModelClass  => commonPrefix(x)
      case x: ModelTrait  => commonPrefix(x)
      case x: ModelObject => commonPrefix(x)
      case x: ModelFields => commonPrefix(x) ::: List(x.syntheticName, x.isLazy, x.fieldCount)
      case x: ModelVal    => commonPrefix(x) ::: fieldCommon(x) ::: List(x.isLazy)
      case x: ModelVar    => commonPrefix(x) ::: fieldCommon(x)
      case x: ModelSource => commonPrefix(x) ::: List(x.encoding, x.length, x.javaHash, x.murmurHash)
    }
    val msg = fields.mkString(",")

    if (!writer.writeLine(msg))
      logger.scopeLog(s" -->[DUPLICATE_ELE] $msg")

    if (logger.debug)
      logger.scopeLog(s" -->[ELEMENT] $msg")

  }

}
