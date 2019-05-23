package scalaclean.model.impl.v2

import scalaclean.model
import scalaclean.model.impl.{ExtendsImpl, OverridesImpl, RefersImpl, WithinImpl}
import scalaclean.model.{Extends, Overrides, Reference, Refers}
import scalafix.v1.{Symbol, SymbolInformation}

import scala.meta.{Decl, Defn, Tree}
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
    extnds.values.foreach(_.foreach(_.complete(elements)))
    overrides.values.foreach(_.foreach(_.complete(elements)))
    within.values.foreach(_.foreach(_.complete(elements)))
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
    refersTo map {
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
    overriden collect {
      case o if o.isDirect => o.fromElement
    }
  }

  override def internalTransitiveOverriddenBy: List[model.ModelElement] =  {
    overriden map {_.fromElement}
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
      case e if e.isDirect => e.fromElement
    }) toSet
  }

  override def transitiveExtendedBy: Set[model.ClassLike] = {
    (extendedBy map {_.fromElement
    }) toSet
  }

}

import scalaclean.model.impl.v2.ModelHooks._

private[impl] abstract class ElementModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ModelElement
 with LegacyReferences with LegacyOverrides{
  def complete(elements: Map[Symbol, ElementModelImpl],
               relsFrom: BasicRelationshipInfo,
               relsTo: BasicRelationshipInfo): Unit = {
    within = relsFrom.within.getOrElse(symbol, Nil) map {
      _.toElement.get.asInstanceOf[ElementModelImpl]
    }
    children = relsTo.within.getOrElse(symbol, Nil) map {
      _.fromElement.asInstanceOf[ElementModelImpl]
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

  override def symbolInfo: SymbolInformation = project.symbolInfo(this, symbol)

  override def symbolInfo(anotherSymbol: Symbol): SymbolInformation = project.symbolInfo(this, anotherSymbol)

  override def fields: List[model.FieldModel] = children collect {
    case f: FieldModelImpl => f
  }

  override def methods: List[model.MethodModel] = children collect {
    case m: MethodModelImpl => m
  }

  override def innerClassLike: Seq[model.ClassLike] = children collect {
    case c: ClassLikeModelImpl => c
  }
  protected def typeName: String

  def treeType: ClassTag[ _ <: Tree]
  def tree = {
    val tag = treeType
    source.treeAt(offsetStart, offsetEnd)(tag)
  }
  private val offsetStart = info.startPos
  private val offsetEnd = info.endPos
  override protected def infoPosString: String = {
    val pos = tree.pos
    s"${pos.startLine}:${pos.startColumn} - ${pos.endLine}:${pos.endColumn}"

  }
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

object ClassModelImpl {
  val treeType = ClassTag[Defn.Class](classOf[Defn.Class])
}
class ClassModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
  extends ClassLikeModelImpl(info, relationships) with ClassModel {
  override protected def typeName: String = "class"

  override def treeType: ClassTag[Defn.Class] = ClassModelImpl.treeType
}

object ObjectModelImpl {
  val treeType = ClassTag[Defn.Object](classOf[Defn.Object])
}
class ObjectModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
  extends ClassLikeModelImpl(info, relationships) with ObjectModel{
  override protected def typeName: String = "object"

  override def treeType: ClassTag[Defn.Object] = ObjectModelImpl.treeType
}

object TraitModelImpl {
  val treeType = ClassTag[Defn.Trait](classOf[Defn.Trait])
}
class TraitModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
  extends ClassLikeModelImpl(info, relationships) with TraitModel{
  override protected def typeName: String = "trait"

  override def treeType: ClassTag[Defn.Trait] = TraitModelImpl.treeType
}

object MethodModelImpl {
  val treeAbstractType = ClassTag[Decl.Def](classOf[Decl.Def])
  val treeConcreteType = ClassTag[Defn.Def](classOf[Defn.Def])
}
class MethodModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                      val methodName: String, val isAbstract: Boolean, val hasDeclaredType: Boolean)
  extends ElementModelImpl(info, relationships) with MethodModel{
  override protected def typeName: String = "def"

  override def treeType: ClassTag[_ <: Tree] =
    if (isAbstract) MethodModelImpl.treeAbstractType
    else MethodModelImpl.treeConcreteType
}

object ValModelImpl {
  val treeAbstractType = ClassTag[Decl.Val](classOf[Decl.Val])
  val treeConcreteType = ClassTag[Defn.Val](classOf[Defn.Val])
}
class ValModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                   fieldName: String, isAbstract: Boolean, val isLazy: Boolean)
  extends FieldModelImpl(info, relationships, fieldName, isAbstract) with ValModel {
  override protected def typeName: String = "trait"

  protected override def infoDetail = s"${super.infoDetail}lazy=$isLazy"

  override def treeType: ClassTag[_ <: Tree] =
    if (isAbstract) ValModelImpl.treeAbstractType
    else ValModelImpl.treeConcreteType
}

object VarModelImpl {
  val treeAbstractType = ClassTag[Decl.Var](classOf[Decl.Var])
  val treeConcreteType = ClassTag[Defn.Var](classOf[Defn.Var])
}
class VarModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                   fieldName: String, isAbstract: Boolean)
  extends FieldModelImpl(info, relationships, fieldName, isAbstract) with VarModel{
  override protected def typeName: String = "var"

  override def treeType: ClassTag[_ <: Tree] =
    if (isAbstract) VarModelImpl.treeAbstractType
    else VarModelImpl.treeConcreteType
}

