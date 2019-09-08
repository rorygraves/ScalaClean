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

  def finish(): Unit = {
    pr.close()
  }

}
