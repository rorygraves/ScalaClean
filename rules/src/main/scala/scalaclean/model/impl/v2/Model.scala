package scalaclean.model.impl.v2

import java.nio.file.{Path, Paths}
import java.util.concurrent.ConcurrentHashMap

import scalaclean.model
import scalaclean.model.impl.{ExtendsImpl, OverridesImpl, RefersImpl, WithinImpl}
import scalaclean.model.{Extends, Overrides, Reference, Refers, Within}
import scalafix.v1.{Symbol, SymbolInformation}

import scala.meta.Tree
import scala.reflect.ClassTag


object ModelHooks {

  import scalaclean.model.impl.{ClassLikeHook, ClassModelHook, FieldModelHook, MethodModelHook, ModelElementHook, ObjectModelHook, TraitModelHook, ValModelHook, VarModelHook}

  type ModelElement = ModelElementHook
  type ClassLike = ClassLikeHook
  type ClassModel = ClassModelHook
  type ObjectModel = ObjectModelHook
  type TraitModel = TraitModelHook
  type MethodModel = MethodModelHook
  type FieldModel = FieldModelHook
  type ValModel = ValModelHook
  type VarModel = VarModelHook
}

private[v2] case class BasicElementInfo(symbol: Symbol, source: SourceData, startPos: Int, endPos: Int)
private[v2] case class BasicRelationshipInfo(
                                              refers: Map[Symbol, List[RefersImpl]],
                                              extnds: Map[Symbol, List[ExtendsImpl]],
                                              overrides: Map[Symbol, List[OverridesImpl]],
                                              within: Map[Symbol, List[WithinImpl]]) {
  def complete(elements: Map[Symbol, ElementModelImpl]): Unit ={
    refers.values.foreach(_.foreach(_.complete(elements)))
  }

  def byTo = {
    def byToSymbol[T <: Reference](from: Map[Symbol, List[T]]): Map[Symbol, List[T]] = {
      from.values.flatten.groupBy(_.toSymbol).map {
        case (k, v) => k -> (v.toList)
      }
    }

    BasicRelationshipInfo(
      byToSymbol(refers),
      byToSymbol(extnds),
      byToSymbol(overrides),
      byToSymbol(within)
    )

  }

  def + (that: BasicRelationshipInfo): BasicRelationshipInfo = {
    val res = BasicRelationshipInfo(
      this.refers ++ that.refers,
      this.extnds ++ that.extnds,
      this.overrides ++ that.overrides,
      this.within ++ that.within
    )
    //there should be no overlaps
    assert(res.refers.size == this.refers.size + that.refers.size)
    assert(res.extnds.size == this.extnds.size + that.extnds.size)
    assert(res.overrides.size == this.overrides.size + that.overrides.size)
    assert(res.within.size == this.within.size + that.within.size)

    res
  }
}


import scalaclean.model.impl.v2.ModelHooks._

abstract class ElementModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ModelElement {
  def complete: Unit = {}

  val source = info.source
  def project = source.project
  def projects = project.projects

  override val symbol = info.symbol

  override def name: String = ???

  override def enclosing: List[ElementModelImpl] = ???

  override def classOrEnclosing: ClassLike = enclosing.head.classOrEnclosing

  override def internalOutgoingReferences: List[(model.ModelElement, Refers)] = ???

  override def internalIncomingReferences: List[(model.ModelElement, Refers)] = ???

  override def allOutgoingReferences: List[(Option[model.ModelElement], Refers)] = ???

  override def internalDirectOverrides: List[model.ModelElement] = ???

  override def internalTransitiveOverrides: List[model.ModelElement] = ???

  override def allDirectOverrides: List[(Option[model.ModelElement], Symbol)] = ???

  override def allTransitiveOverrides: List[(Option[model.ModelElement], Symbol)] = ???

  override def internalDirectOverriddenBy: List[model.ModelElement] = ???

  override def internalTransitiveOverriddenBy: List[model.ModelElement] = ???

  override def symbolInfo: SymbolInformation = ???

  override def symbolInfo(anotherSymbol: Symbol): SymbolInformation = ???

  override def fields: List[model.FieldModel] = ???

  override def methods: List[model.MethodModel] = ???

  override def innerClassLike: Seq[model.ClassLike] = ???

  override protected def infoTypeName: String = ???

  override protected def infoPosString: String = ???
}

abstract class ClassLikeModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ElementModelImpl(info, relationships) with ClassLike {
  override def classOrEnclosing: ClassLike = this
  override def fullName: String = ???

  override def xtends[T](implicit cls: ClassTag[T]): Boolean = ???

  override def xtends(symbol: Symbol): Boolean = ???

  override def directExtends: Set[Symbol] = ???

  override def transitiveExtends: Set[Symbol] = ???

  override def directExtendedBy: Set[model.ClassLike] = ???

  override def transitiveExtendedBy: Set[model.ClassLike] = ???
}

abstract class FieldModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ElementModelImpl(info, relationships) with FieldModel {
  override def otherFieldsInSameDeclaration: Seq[fieldType] = ???
}

class ClassModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ClassLikeModelImpl(info, relationships) with ClassModel

class ObjectModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ClassLikeModelImpl(info, relationships) with ObjectModel

class TraitModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ClassLikeModelImpl(info, relationships) with TraitModel

class MethodModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ElementModelImpl(info, relationships) with MethodModel

class ValModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends FieldModelImpl(info, relationships) with ValModel {
  override def isLazy: Boolean = ???

}

class VarModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends FieldModelImpl(info, relationships) with VarModel

