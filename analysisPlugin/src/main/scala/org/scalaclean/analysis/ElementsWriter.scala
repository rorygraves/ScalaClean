package org.scalaclean.analysis

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

class ElementsWriter(file: File) {

  val  pr = new PrintWriter(new BufferedWriter(new FileWriter(file)))

  def method(isGlobal: Boolean,symbolName: String, file: String, start: Int, end: Int): Unit = {
    val globalStr = if(isGlobal) "G:" else "L:"
    pr.println(s"${IoTokens.typeMethod},$globalStr$symbolName,$file,$start,$end")

  }

  def classDef(isGlobal: Boolean,symbolName: String, file: String, start: Int, end: Int): Unit = {
    val globalStr = if(isGlobal) "G:" else "L:"
    pr.println(s"${IoTokens.typeClass},$globalStr$symbolName,$file,$start,$end")
  }

  def valDef(isGlobal: Boolean,symbolName: String, file: String, start: Int, end: Int, isAbstract: Boolean, fieldName: String, isLazy: Boolean): Unit = {
    val globalStr = if (isGlobal) "G:" else "L:"
    pr.println(s"${IoTokens.typeVal},$globalStr$symbolName,$file,$start,$end,$isAbstract,$fieldName,$isLazy")

  }

  def varDef(isGlobal: Boolean,symbolName: String, file: String, start: Int, end: Int, isAbstract: Boolean, fieldName: String): Unit = {
    val globalStr = if (isGlobal) "G:" else "L:"
    pr.println(s"${IoTokens.typeVar},$globalStr$symbolName,$file,$start,$end,$isAbstract,$fieldName")

  }

  def finish(): Unit = {
    pr.close()
  }

}
