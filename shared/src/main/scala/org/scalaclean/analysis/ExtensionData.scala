package org.scalaclean.analysis

import java.util.concurrent.ConcurrentHashMap

/**
  * should generally be the companion of the [[ExtensionData]] case class
  *
  * @tparam T the companion
  */
abstract class ExtensionDescriptor[T <: ExtensionData] {
  def clearData = interner.clear
  private val interner = new ConcurrentHashMap[String, T]()
  def fromCsv(s:String): T = {
    interner.computeIfAbsent(s, build)
  }
  protected def build(s:String): T
}

trait ExtensionData extends Product {
  def toCsv: String = productIterator.mkString("", ",", "")
}

trait StandardExtensionData extends ExtensionData {
  /**
    * the start offset from the element
    * Note - we use offsets to promote reuse
    */
  def posOffsetStart: Int
  /**
    * the end offset from the element
    * Note - we use offsets to promote reuse
    */
  def posOffsetEnd: Int
}




