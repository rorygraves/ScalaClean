package org.scalaclean.analysis

case class ModelSymbol(
  isGlobal: Boolean, semanticRep: String, sourceFile: String, posStart: Int, posEnd: Int, isSynthetic: Boolean,
  isAbstract: Boolean, isLazy: Boolean, name: String) {

  def csvString: String = {
    val globalStr = if(isGlobal) "G:" else "L:"
    s"$globalStr$semanticRep"
  }

}
