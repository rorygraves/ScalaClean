package scalaclean.model

import scalaclean.model.impl.v1.ScalaCleanModelImpl
import scalafix.v1.{SemanticDocument, Symbol, SymbolInformation}

import scala.reflect.ClassTag

sealed trait ModelElement {
  def symbol: Symbol

  var mark : Mark = _
  def name: String

  //usually just one element. Can be >1 for  RHS of a val (a,b,c) = ...
  //where a,b,c are the enclosing
  def enclosing: List[ModelElement]
  def classOrEnclosing: ClassLike

  def internalOutgoingReferences: List[(ModelElement, RefersTo)]
  def internalIncomingReferences: List[(ModelElement, RefersTo)]
  def allOutgoingReferences: List[(Option[ModelElement], RefersTo)]

  def internalDirectOverrides: List[ModelElement]
  def internalTransitiveOverrides: List[ModelElement]

  def allDirectOverrides: List[(Option[ModelElement], Symbol)]
  def allTransitiveOverrides: List[(Option[ModelElement], Symbol)]

  def internalDirectOverriddenBy: List[ModelElement]
  def internalTransitiveOverriddenBy: List[ModelElement]

  def symbolInfo: SymbolInformation
  def symbolInfo(anotherSymbol: Symbol): SymbolInformation

  //any block may contain many val of the same name!
  //  val foo = {
  //    if (1 == 1) {
  //      val x = 2
  //      x
  //    } else {
  //      val x = 3
  //      x
  //    }
  //  }
  def fields: List[FieldModel]
  def methods:  List[MethodModel]
  def innerClassLike: Seq[ClassLike]


  protected def infoTypeName: String
  protected def infoPosString: String
  protected def infoDetail = ""
  protected def infoName = symbol.displayName

  override def toString: String = s"$infoTypeName $infoName [$infoPosString] $infoDetail"
}

sealed trait ClassLike extends ModelElement {
  def fullName: String

  def xtends[T](implicit cls: ClassTag[T]): Boolean
  def xtends(symbol: Symbol): Boolean

  def directExtends: Set[Symbol]
  def transitiveExtends: Set[Symbol]

  def directExtendedBy: Set[ClassLike]
  def transitiveExtendedBy: Set[ClassLike]
}

sealed trait ClassModel extends ClassLike {
  override protected final def infoTypeName: String = "ClassModel"
}
sealed trait ObjectModel extends ClassLike with FieldModel{
  override protected final def infoTypeName: String = "ObjectModel"
  final override def otherFieldsInSameDeclaration = Nil
  type fieldType  = ObjectModel
}

sealed trait TraitModel extends ClassLike{
  override protected final def infoTypeName: String = "TraitModel"
}

sealed trait MethodModel extends ModelElement {
  override protected final def infoTypeName: String = "MethodModel"
}

sealed trait FieldModel extends ModelElement{
  type fieldType <: FieldModel
  def otherFieldsInSameDeclaration: Seq[fieldType]
}

sealed trait ValModel extends FieldModel{
  def isLazy: Boolean
  type fieldType = ValModel
  override protected final def infoTypeName: String = "ValModel"
}

sealed trait VarModel extends FieldModel{
  type fieldType = VarModel
  override protected final def infoTypeName: String = "VarModel"
}
//hooks for migration
package impl {
  type me  = ModelElement
  trait ModelElementHook extends ModelElement
  trait ClassLikeHook extends ClassLike with ModelElementHook
  trait ClassModelHook extends ClassModel with ClassLikeHook
  trait ObjectModelHook extends ObjectModel with ClassLikeHook
  trait TraitModelHook extends TraitModel with ClassLikeHook
  trait MethodModelHook extends MethodModel with ModelElementHook
  trait FieldModelHook extends FieldModel with ModelElementHook
  trait ValModelHook extends ValModel with FieldModelHook
  trait VarModelHook extends VarModel with FieldModelHook
}
object ScalaCleanModel {
  private val version = 1
  def createParseModel: ParseModel = new ScalaCleanModelImpl
}
trait ParseModel {
  def analyse(implicit doc: SemanticDocument): Unit
  def finishedParsing(): Unit
  def asProjectModel: ProjectModel
}
trait ProjectModel {
  def fromSymbol[T <: ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): T

  def getSymbol[T <: ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): Option[T]

  def size: Int
  def allOf[T <: ModelElement : ClassTag]: List[T]
  def printStructure() = allOf[ClassLike] foreach {
    cls => println(s"class ${cls.fullName}")
  }


}
trait ScalaCleanModel extends ParseModel with ProjectModel {
  def asProjectModel: ProjectModel = this
}