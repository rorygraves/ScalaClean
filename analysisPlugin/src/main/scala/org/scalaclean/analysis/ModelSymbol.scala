package org.scalaclean.analysis

import scala.tools.nsc.Global

trait HasModelCommon {
  def common: ModelCommon
  def csvString: String = common.csvString
  def newCsvString: String = common.newCsvString
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
  val tree: Global#Tree
  def gTree(g: Global): g.Tree = tree.asInstanceOf[g.Tree]
  private var _data: List[ExtensionData] = Nil
  def addData (additionalData: List[ExtensionData]): Unit = {
    if (_data .isEmpty) _data = additionalData
    else _data = _data.reverse_:::(additionalData)
  }
  def data = _data

  def ioToken: String
}
sealed abstract class ModelField extends ModelSymbol
case class ModelCommon(isGlobal: Boolean, id:String, newId: String, sourceFile: String, posStart: Int,posEnd: Int, sourceName:String) extends HasModelCommon {
  override def common: ModelCommon = this
  override def csvString: String = {
    val globalStr = if(isGlobal) "G:" else "L:"
    s"$globalStr$id"
  }
  override def newCsvString: String = newId
}

case class ModelVar(tree: Global#ValDef, common: ModelCommon, isAbstract: Boolean) extends ModelField{
  override def debugName: String = "var"

  override def ioToken: String = IoTokens.typeVar
}
case class ModelVal(tree: Global#ValDef, common: ModelCommon, isAbstract: Boolean, isLazy: Boolean) extends ModelField{
  override def debugName: String = "val"
  override def ioToken: String = IoTokens.typeVal
}

sealed trait ModelMethod extends ModelSymbol {
  val tree: Global#DefDef
  val common: ModelCommon
  val isTyped: Boolean
  val isAbstract: Boolean
}
case class ModelGetterMethod(tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelMethod {
  override def debugName: String = "getter"
  override def ioToken: String = IoTokens.typeGetterMethod
}

case class ModelSetterMethod(tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelMethod {
  override def debugName: String = "setter"
  override def ioToken: String = IoTokens.typeSetterMethod
}

case class ModelPlainMethod(tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelMethod {
  override def debugName: String = "def"
  override def ioToken: String = IoTokens.typePlainMethod
}
case class ModelObject(tree: Global#ModuleDef, common: ModelCommon) extends ModelSymbol{
  override def debugName: String = "object"
  override def ioToken: String = IoTokens.typeObject
}
case class ModelClass(tree: Global#ClassDef, common: ModelCommon, isAbstract: Boolean) extends ModelSymbol{
  override def debugName: String = "class"
  override def ioToken: String = IoTokens.typeClass
}
case class ModelTrait(tree: Global#ClassDef, common: ModelCommon) extends ModelSymbol{
  override def debugName: String = "trait"
  override def ioToken: String = IoTokens.typeTrait
}

case class ModelSource(tree: Global#Tree, common: ModelCommon) extends ModelSymbol{
  override def debugName: String = "source"
  override def ioToken: String = IoTokens.typeSource
}
