package org.scalaclean.analysis

trait HasModelCommon {
  def common: ModelCommon
  def csvString: String = common.csvString

}

sealed trait ModelSymbol extends HasModelCommon{
  def debugName: String
  val common: ModelCommon
  def isGlobal: Boolean = common.isGlobal
  def semanticRep: String = common.id
  def  sourceFile: String = common.sourceFile
  def  posStart: Int = common.posStart
  def  posEnd: Int = common.posEnd
//  def  isSynthetic: Boolean
//  def  isAbstract: Boolean
//  def  isLazy: Boolean
  def  sourceName: String = common.sourceName

}
sealed abstract class ModelField extends ModelSymbol
case class ModelCommon(isGlobal: Boolean, id:String, sourceFile: String, posStart: Int,posEnd: Int, sourceName:String) extends HasModelCommon {
  override def common: ModelCommon = this
  override def csvString: String = {
    val globalStr = if(isGlobal) "G:" else "L:"
    s"$globalStr$id"
  }
}

case class ModelVar(common: ModelCommon, isAbstract: Boolean) extends ModelField{
  override def debugName: String = "var"
}
case class ModelVal(common: ModelCommon, isAbstract: Boolean, isLazy: Boolean) extends ModelField{
  override def debugName: String = "val"
}

sealed trait ModelMethod extends ModelSymbol {
  val common: ModelCommon
  val isTyped: Boolean
  val isAbstract: Boolean

  def ioToken: String

}
case class ModelGetterMethod(common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelMethod {
  override def debugName: String = "getter"
  override def ioToken: String = IoTokens.typeGetterMethod
}

case class ModelSetterMethod(common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelMethod {
  override def debugName: String = "setter"
  override def ioToken: String = IoTokens.typeSetterMethod
}

case class ModelPlainMethod(common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelMethod {
  override def debugName: String = "def"
  override def ioToken: String = IoTokens.typePlainMethod
}
case class ModelObject(common: ModelCommon) extends ModelSymbol{
  override def debugName: String = "object"
}
case class ModelClass(common: ModelCommon, isAbstract: Boolean) extends ModelSymbol{
  override def debugName: String = "class"
}
case class ModelTrait(common: ModelCommon) extends ModelSymbol{
  override def debugName: String = "trait"
}

case class ModelSource(common: ModelCommon) extends ModelSymbol{
  override def debugName: String = "source"
}
