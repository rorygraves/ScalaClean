package org.scalaclean.analysis

import java.io.File

class ElementsWriter(file: File) {

  val writer = new SortedStringWriter(file.toPath)

  def method(mSymbol: ModelSymbol, methodName: String, declTypeDefined: Boolean): Unit = {
    writer.writeLine(s"${IoTokens.typeMethod},${mSymbol.csvString},${mSymbol.sourceFile},${mSymbol.posStart},${mSymbol.posEnd},${mSymbol.isAbstract},$methodName,$declTypeDefined")
  }

  def method(
    isGlobal: Boolean, symbolName: String, file: String, start: Int, end: Int, isAbstract: Boolean, methodName: String,
    declTypeDefined: Boolean): Unit = {
    val globalStr = if (isGlobal) "G:" else "L:"
    writer.writeLine(s"${IoTokens.typeMethod},$globalStr$symbolName,$file,$start,$end,$isAbstract,$methodName,$declTypeDefined")
  }

  def classDef(mSymbol: ModelSymbol): Unit = {
    writer.writeLine(s"${IoTokens.typeClass},${mSymbol.csvString},${mSymbol.sourceFile},${mSymbol.posStart},${mSymbol.posEnd}")
  }

  def traitDef(mSymbol: ModelSymbol): Unit = {
    writer.writeLine(s"${IoTokens.typeTrait},${mSymbol.csvString},${mSymbol.sourceFile},${mSymbol.posStart},${mSymbol.posEnd}")
  }

  def objectDef(isGlobal: Boolean, symbolName: String, file: String, start: Int, end: Int): Unit = {
    val globalStr = if (isGlobal) "G:" else "L:"
    writer.writeLine(s"${IoTokens.typeObject},$globalStr$symbolName,$file,$start,$end")
  }

  def valDef(mSymbol: ModelSymbol): Unit = {
    valVarDef(IoTokens.typeVal, mSymbol, s",${mSymbol.isLazy}")
  }

  def varDef(mSymbol: ModelSymbol): Unit = {
    valVarDef(IoTokens.typeVar, mSymbol)
  }

  private def valVarDef(token: String, mSymbol: ModelSymbol, extraStr: String = ""): Unit = {
    writer.writeLine(s"${token},${mSymbol.csvString},${mSymbol.sourceFile},${mSymbol.posStart},${mSymbol.posEnd},${mSymbol.isAbstract},${mSymbol.name}$extraStr")
  }

  def finish(): Unit = {
    writer.close()
  }

}
