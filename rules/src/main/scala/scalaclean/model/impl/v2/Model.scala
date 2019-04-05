package scalaclean.model.impl.v2

import scalaclean.model
import scalaclean.model.impl.{ExtendsImpl, OverridesImpl, RefersImpl, WithinImpl}
import scalaclean.model.{Extends, Overrides, Reference, Refers}
import scalafix.v1.{Symbol, SymbolInformation}

import scala.reflect.ClassTag


object ModelHooks {

  import scalaclean.model.impl._

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

private[v2] trait LegacyReferences {
  self: ElementModelImpl =>

  override def internalOutgoingReferences: List[(model.ModelElement, Refers)] = {
    refersTo.collect {
      case r  if r.toElement.isDefined => (r.toElement.get, r)
    }
  }

  override def internalIncomingReferences: List[(model.ModelElement, Refers)] = {
    refersFrom.collect {
      case r => (r.fromElement, r)
    }
  }

  override def allOutgoingReferences: List[(Option[model.ModelElement], Refers)] = {
    refersFrom map {
      r => (r.toElement, r)
    }
  }
}
private[v2] trait LegacyOverrides {
  self: ElementModelImpl =>

  override def internalDirectOverrides: List[model.ModelElement] = {
    overrides collect {
      case o if o.isDirect && o.toElement.isDefined => o.toElement.get
    }
  }

  override def internalTransitiveOverrides: List[model.ModelElement] = {
    overrides collect {
      case o if o.toElement.isDefined => o.toElement.get
    }
  }

  override def allDirectOverrides: List[(Option[model.ModelElement], Symbol)] = {
    overrides collect {
      case o if o.isDirect => (o.toElement, o.toSymbol)
    }
  }

  override def allTransitiveOverrides: List[(Option[model.ModelElement], Symbol)] = {
    overrides collect {
      case o => (o.toElement, o.toSymbol)
    }
  }

  override def internalDirectOverriddenBy: List[model.ModelElement] =  {
    overrides collect {
      case o if o.isDirect => o.fromElement
    }
  }

  override def internalTransitiveOverriddenBy: List[model.ModelElement] =  {
    overrides collect {
      case o if o.toElement.isDefined => o.toElement.get
    }
  }
}
private[v2] trait LegacyExtends {
  self: ClassLikeModelImpl=>

  override def directExtends: Set[Symbol] =  {
    extnds collect {
      case s if s.isDirect => s.toSymbol
    } toSet
  }

  override def transitiveExtends: Set[Symbol] = {
    extnds map (_.toSymbol) toSet
  }

  override def directExtendedBy: Set[model.ClassLike] = {
    (extendedBy collect {
      case e if e.isDirect => e.toElement.get
    }) toSet
  }

  override def transitiveExtendedBy: Set[model.ClassLike] = ???

}

import scalaclean.model.impl.v2.ModelHooks._

private[impl] abstract class ElementModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ModelElement
 with LegacyReferences with LegacyOverrides{
  def complete(elements: Map[Symbol, ElementModelImpl],
               relsFrom: BasicRelationshipInfo,
               relsTo: BasicRelationshipInfo): Unit = {
    within = relsFrom.within.getOrElse(symbol, Nil) map {
      _.fromElement.asInstanceOf[ElementModelImpl]
    }
    children = relsTo.within.getOrElse(symbol, Nil) map {
      _.toElement.asInstanceOf[ElementModelImpl]
    }
    refersTo = relsFrom.refers.getOrElse(symbol, Nil)
    refersFrom = relsTo.refers.getOrElse(symbol, Nil)
    _overrides = relsFrom.overrides.getOrElse(symbol, Nil)
    overriden = relsTo.overrides.getOrElse(symbol, Nil)
  }

  val source = info.source
  def project = source.project
  def projects = project.projects

  override val symbol = info.symbol

  override def name = symbol.displayName

  //start set by `complete`
  var within: List[ElementModelImpl] = _
  var children: List[ElementModelImpl] = _

  var refersTo: List[Refers] = _
  var refersFrom: List[Refers] = _

  var _overrides: List[Overrides] = _
  override def overrides = _overrides
  var overriden: List[Overrides] = _
  //end set by `complete`

  override def enclosing: List[ElementModelImpl] = within

  override def classOrEnclosing: ClassLike = enclosing.head.classOrEnclosing

  override def symbolInfo: SymbolInformation = project.symbolInfo(symbol)

  override def symbolInfo(anotherSymbol: Symbol): SymbolInformation = project.symbolInfo(anotherSymbol)

  override def fields: List[model.FieldModel] = children collect {
    case f: FieldModelImpl => f
  }

  override def methods: List[model.MethodModel] = children collect {
    case m: MethodModelImpl => m
  }

  override def innerClassLike: Seq[model.ClassLike] = children collect {
    case c: ClassLikeModelImpl => c
  }

  override protected def infoTypeName: String = ???

  override protected def infoPosString: String = ???
}

abstract class ClassLikeModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ElementModelImpl(info, relationships)
  with ClassLike with LegacyExtends {

  var extnds: List[Extends] = _
  var extendedBy: List[Extends] = _

  override def complete(elements: Map[Symbol, ElementModelImpl],
                        relsFrom: BasicRelationshipInfo,
                        relsTo: BasicRelationshipInfo): Unit = {
    super.complete(elements, relsFrom, relsTo)
    extnds = relsFrom.extnds.getOrElse(symbol, Nil)
    extendedBy = relsTo.extnds.getOrElse(symbol, Nil)

  }

  override def classOrEnclosing: ClassLike = this

  override def fullName: String = ???

  override def xtends[T](implicit cls: ClassTag[T]): Boolean = {
    //TODO what is the canonocal conversion?
    xtends(Symbol(cls.runtimeClass.getName.replace('.', '/') + "#"))
  }
  override def xtends(symbol: Symbol): Boolean = {
    extnds.exists (_.toSymbol == symbol)
  }

}

abstract class FieldModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                              val fieldName: String, val isAbstract: Boolean)
  extends ElementModelImpl(info, relationships) with FieldModel {
  override def otherFieldsInSameDeclaration: Seq[fieldType] = ???
}

class ClassModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
  extends ClassLikeModelImpl(info, relationships) with ClassModel

class ObjectModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
  extends ClassLikeModelImpl(info, relationships) with ObjectModel

class TraitModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
  extends ClassLikeModelImpl(info, relationships) with TraitModel

class MethodModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                      val methodName: String, val isAbstract: Boolean, val hasDeclaredType: Boolean)
  extends ElementModelImpl(info, relationships) with MethodModel

class ValModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                   fieldName: String, isAbstract: Boolean, val isLazy: Boolean)
  extends FieldModelImpl(info, relationships, fieldName, isAbstract) with ValModel {

}

class VarModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                   fieldName: String, isAbstract: Boolean)
  extends FieldModelImpl(info, relationships, fieldName, isAbstract) with VarModel

