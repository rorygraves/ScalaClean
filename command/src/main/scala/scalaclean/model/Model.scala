package scalaclean.model

import org.scalaclean.analysis.{ AnnotationData, ExtensionData }
import scalaclean.model.impl.ElementModelImpl

import scala.reflect.ClassTag
import scala.tools.nsc.symtab.Flags

import Filters._
import InternalFilters._

sealed trait ModelElement extends Ordered[ModelElement] {

  override def compare(that: ModelElement): Int = modelElementId.id.compare(that.modelElementId.id)

  def modelElementId: ElementId

  var _mark: Mark[_] = null
  def mark = _mark
  def mark_=[T <: SomeSpecificColour](newMark: Mark[T]): Unit = {
    if (_mark eq null)
      _mark = newMark
    else {
      assert (!newMark.isInitial)
      _mark = _mark.asInstanceOf[Mark[T]].merge(newMark)
    }
  }

  def name: String

  def flags: Long
  final def isFinal       = (Flags.FINAL & flags) != 0
  final def isPrivate     = (Flags.PRIVATE & flags) != 0
  final def isPrivateThis = (Flags.PrivateLocal & flags) == Flags.PrivateLocal

  //usually just one element. Can be >1 for  RHS of a val (a,b,c) = ...
  //where a,b,c are the enclosing
  def enclosing: List[ModelElement]

  def classOrEnclosing: ClassLike

  def annotationsOf(cls: Class[_]): Iterable[AnnotationData] = annotations.filter(_.fqName == cls.getName)

  def annotations: Iterable[AnnotationData] = extensions.collect { case a: AnnotationData => a }

  def extensions: Iterable[ExtensionData]

  def extensionsOfType[T <: ExtensionData: ClassTag]: Iterable[T] = {
    extensions.collect { case a: T => a }
  }

  def extensionOfType[T <: ExtensionData: ClassTag]: Option[T] = {
    extensions.collectFirst { case a: T => a }
  }

  //start target APIs
  // def outgoingReferences: Iterable[Refers] = allOutgoingReferences map (_._2)

  def overrides: Iterable[Overrides]
  def overridden: Iterable[Overrides]

  //should be in the following form
  def outgoingReferences: Iterable[Refers]

  //  def outgoingReferencedInternal[T <: ModelElement]: Iterable[T]
  //  def outgoingReferencedExternal: Iterable[Symbol]
  //
  def incomingReferences: Iterable[Refers]

  //  def incomingReferencedInternal[T <: ModelElement]: Iterable[T]

  //end target APIs

  //start old APIs
  def internalOutgoingReferences: List[(ModelElement, Refers)]

  def internalIncomingReferences: List[(ModelElement, Refers)]

  def allOutgoingReferences: List[(Option[ModelElement], Refers)]

  def internalDirectOverrides: List[ModelElement]

  def internalTransitiveOverrides: List[ModelElement]

  def allDirectOverrides: List[(Option[ModelElement], ElementId)]

  def allTransitiveOverrides: List[(Option[ModelElement], ElementId)]

  def internalDirectOverriddenBy: List[ModelElement]

  def internalTransitiveOverriddenBy: List[ModelElement]

  //end old APIs

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

  def methods: List[MethodModel]

  def innerClassLike: Seq[ClassLike]

  def allChildren: List[ElementModelImpl]

  def isAbstract: Boolean

  final protected def infoTypeName: String = this match {
    case _: ClassModel        => "ClassModel"
    case _: ObjectModel       => "ObjectModel"
    case _: TraitModel        => "TraitModel"
    case _: GetterMethodModel => "GetterMethodModel"
    case _: SetterMethodModel => "SetterMethodModel"
    case _: PlainMethodModel  => "PlainMethodModel"
    case _: ValModel          => "ValModel"
    case _: VarModel          => "VarModel"
    case _: FieldsModel       => "FieldsModel"
    case _: SourceModel       => "SourceModel"
  }

  protected def infoPosString: String

  final def infoPosSorted: (String, Int, Int) = (sourceFileName, rawStart, rawEnd)

  def rawFocusStart: Int

  def rawStart: Int

  def rawEnd: Int

  def existsInSource: Boolean

  def sourceFileName: String

  protected def infoDetail = ""

  protected def infoName: String = modelElementId.id

  override def toString: String = s"$infoTypeName $infoName [$infoPosString] $infoDetail [[$modelElementId]]"
}

sealed trait ClassLike extends ModelElement {
  def getCompanionObject(model: ProjectModel): Option[ObjectModel] = {
    val ele = modelElementId.companionObjectOrSelf
    if (ele == modelElementId) None
    else Some(model.element[ObjectModel](ele))
  }

  def fullName: String

  def xtends[T](implicit cls: ClassTag[T]): Boolean

  def xtends(symbol: ElementId): Boolean

  protected def extnds: Seq[Extends]
  protected def extendedBy: Seq[Extends]

  private def extendsClassLike0(onlyDirect: Boolean, filter: ExtendsClassLike): Iterator[Extends] = {
    var it = extnds.iterator
    if (onlyDirect)
      it = it.filter(OnlyDirect)
    if (filter ne All)
      it = it.filter(e => filter(e.isDirect, e.toElement, e.toElementId))
    it
  }

  /**
   * get the classes and traits that this extends. Note this is limitted to the classes nd traits that are compiled and have scalaclean metadata
   *
   * @see extendsClassLike if you want the ones in libraries as well
   * @param onlyDirect if true, only the direct extensions, otherwise all
   * @param filter     a general purpose filter for the relations to consider
   */
  def extendsClassLikeCompiled(
      onlyDirect: Boolean = false,
      filter: ExtendsClassLikeCompiled = All
  ): Iterator[ClassLike] = {
    extendsClassLike0(onlyDirect, (direct, cls, _) => cls.isDefined && filter(direct, cls.get)).flatMap(_.toElement)
  }

  /**
   * get the classes and traits that this extends
   *
   * @see extendsClassLikeCompiled if you want the ones not from source to be returned
   * @param onlyDirect if true, only the direct extensions, otherwise all
   * @param filter     a general purpose filter for the relations to consider
   * @return an iterator of (Option[ClassLike], ElementId). If the class is None, then this is a library class
   */
  def extendsClassLike(
      onlyDirect: Boolean = false,
      filter: ExtendsClassLike = All
  ): Iterator[(Option[ClassLike], ElementId)] = {
    extendsClassLike0(onlyDirect, filter).map(e => (e.toElement, e.toElementId))
  }

  /**
   * get the object, classes and traits that extend this
   *
   * @param onlyDirect if true, only the direct extensions, otherwise all
   * @param filter     a general purpose filter for the relations to consider
   */
  def extendedByClassLike(onlyDirect: Boolean = false, filter: ExtendedByClassLike = All): Iterator[ClassLike] = {
    var it = extendedBy.iterator
    if (onlyDirect)
      it = it.filter(OnlyDirect)
    if (filter ne All)
      it = it.filter(e => filter(e.isDirect, e.fromElement))
    it.map(_.fromElement)
  }

}

sealed trait ClassModel extends ClassLike

sealed trait ObjectModel extends ClassLike with FieldModel {

  def isTopLevel: Boolean = {
    enclosing.forall(_.isInstanceOf[SourceModel])
  }

  override final def getter: Option[GetterMethodModel]  = None
  override final def accessors: Iterable[AccessorModel] = Nil
  override final def declaredIn: Option[FieldsModel]    = None
  override final def fieldsInSameDeclaration            = Nil

  final type fieldType = ObjectModel
}

sealed trait TraitModel extends ClassLike

sealed trait MethodModel extends ModelElement {
  def methodName: String
}

sealed trait AccessorModel extends MethodModel with FieldOrAccessorModel {
  def field: Option[FieldModel]
}

sealed trait PlainMethodModel extends MethodModel

sealed trait GetterMethodModel extends AccessorModel {
  def field: Option[FieldModel]
}

sealed trait SetterMethodModel extends AccessorModel {
  def field: Option[VarModel]
}

sealed trait FieldOrAccessorModel extends ModelElement

sealed trait FieldModel extends FieldOrAccessorModel {
  type fieldType <: FieldModel

  def getter: Option[GetterMethodModel]
  def accessors: Iterable[AccessorModel]

  def inCompoundFieldDeclaration = declaredIn.isDefined
  def declaredIn: Option[FieldsModel]

  def fieldsInSameDeclaration: List[fieldType]
}

sealed trait FieldsModel extends ModelElement {
  def fieldsInDeclaration: List[FieldModel]
}

sealed trait ValModel extends FieldModel {
  def isLazy: Boolean

  type fieldType = ValModel
}

sealed trait VarModel extends FieldModel {
  type fieldType = VarModel

  def setter: Option[SetterMethodModel]
}

sealed trait SourceModel extends ModelElement {
  type fieldType = SourceModel
}

trait ProjectModel {

  def element[T <: ModelElement](id: ElementId)(implicit tpe: ClassTag[T]): T

  def getElement[T <: ModelElement](id: ElementId)(implicit tpe: ClassTag[T]): Option[T]

  def size: Int

  def allOf[T <: ModelElement: ClassTag]: Iterator[T]

  def printStructure(): Unit = allOf[ClassLike].foreach(cls => println(s"class ${cls.fullName}"))
}

object Filters {

  trait ExtendsClassLike {

    /**
     * a specialised filter for a ClassLike.
     * @see ClassLike.extendsClassLike
     * @see ClassLike.extendsClassLikeCompiled
     * @param direct if true the extension is direct
     * @param asClass None if the related class is a library, or the class if it is compiled in any loaded project
     * @param asElement the related class
     * @return true if the relationship should be included
     */
    def apply(direct: Boolean, asClass: Option[ClassLike], asElement: ElementId): Boolean
  }

  trait ExtendsClassLikeCompiled {

    /**
     * a specialised filter for a ClassLike.
     * @see ClassLike.extendsClassLike
     * @see ClassLike.extendsClassLikeCompiled
     * @param direct if true the extension is direct
     * @param clazz the related class
     * @return true if the relationship should be included
     */
    def apply(direct: Boolean, clazz: ClassLike): Boolean
  }

  trait ExtendedByClassLike {

    /**
     * a specialised filter for a ClassLike.
     * @see ClassLike.extendedByClassLike
     * @param direct if true the extension is direct
     * @param clazz the related class
     * @return true if the relationship should be included
     */
    def apply(direct: Boolean, clazz: ClassLike): Boolean
  }

}

private[model] object InternalFilters {

  object All extends Filters.ExtendsClassLike with Filters.ExtendsClassLikeCompiled with ExtendedByClassLike {
    override def apply(direct: Boolean, asClass: Option[ClassLike], asElement: ElementId): Boolean = true
    override def apply(direct: Boolean, clazz: ClassLike): Boolean                                 = true
  }

  object OnlyDirect extends ((Extends) => Boolean) {
    override def apply(ex: Extends): Boolean = ex.isDirect
  }

}

package impl {

  import java.nio.file.Path

  import org.scalaclean.analysis.FlagHelper

  case class BasicElementInfo(
      elementId: ElementId,
      source: SourceData,
      startPos: Int,
      endPos: Int,
      focusStart: Int,
      flags: Long,
      extensions: Seq[ExtensionData],
      traversal: Int
  ) {

    override def toString: String = {
      s"Info[elementId:$elementId, source:$source, pos:$startPos->$endPos, flags:${FlagHelper.hexAndString(flags)}"
    }

  }

  case class BasicRelationshipInfo(
      refers: Map[ElementId, List[RefersImpl]],
      extnds: Map[ElementId, List[ExtendsImpl]],
      overrides: Map[ElementId, List[OverridesImpl]],
      within: Map[ElementId, List[WithinImpl]],
      getter: Map[ElementId, List[GetterImpl]],
      setter: Map[ElementId, List[SetterImpl]]
  ) {

    def sortValues: BasicRelationshipInfo = {
      BasicRelationshipInfo(
        refers = refers.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) },
        extnds = extnds.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) },
        overrides = overrides.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) },
        within = within.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) },
        getter = getter.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) },
        setter = setter.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) }
      )
    }

    def complete(modelElements: Map[ElementId, ElementModelImpl]): Unit = {
      refers.values.foreach(_.foreach(_.complete(modelElements)))
      extnds.values.foreach(_.foreach(_.complete(modelElements)))
      overrides.values.foreach(_.foreach(_.complete(modelElements)))
      within.values.foreach(_.foreach(_.complete(modelElements)))
      getter.values.foreach(_.foreach(_.complete(modelElements)))
      setter.values.foreach(_.foreach(_.complete(modelElements)))
    }

    def byTo: BasicRelationshipInfo = {
      def byToSymbol[T <: Reference](from: Map[ElementId, List[T]]): Map[ElementId, List[T]] = {
        from.values.flatten.groupBy(_.toElementId).map { case (k, v) => k -> v.toList }
      }

      BasicRelationshipInfo(
        byToSymbol(refers),
        byToSymbol(extnds),
        byToSymbol(overrides),
        byToSymbol(within),
        byToSymbol(getter),
        byToSymbol(setter)
      )

    }

    def +(that: BasicRelationshipInfo): BasicRelationshipInfo = {
      val res = BasicRelationshipInfo(
        this.refers ++ that.refers,
        this.extnds ++ that.extnds,
        this.overrides ++ that.overrides,
        this.within ++ that.within,
        this.getter ++ that.getter,
        this.setter ++ that.setter
      )
      //there should be no overlaps
//      assert(res.refers.size == this.refers.size + that.refers.size)
//      assert(res.extnds.size == this.extnds.size + that.extnds.size)
//      if(res.overrides.size != this.overrides.size + that.overrides.size) {
//        val m1 = res.overrides.keySet
//        val m2 = this.overrides.keySet ++ that.overrides.keySet
//        val diff = (m1 -- m2) ++ (m2 -- m1)
//        println(diff)
//        //assert(res.overrides.size == this.overrides.size + that.overrides.size)
//      }
//      assert(res.overrides.size == this.overrides.size + that.overrides.size)
//      assert(res.within.size == this.within.size + that.within.size)
//      assert(res.getter.size == this.getter.size + that.getter.size)
//      assert(res.setter.size == this.setter.size + that.setter.size)

      res
    }

  }

  trait LegacyReferences {
    self: ElementModelImpl =>

    override def internalOutgoingReferences: List[(ModelElement, Refers)] = {
      _refersTo.collect {
        case r if r.toElement.isDefined => (r.toElement.get, r)
      }
    }

    override def internalIncomingReferences: List[(ModelElement, Refers)] = {
      _refersFrom.collect { case r => (r.fromElement, r) }
    }

    override def allOutgoingReferences: List[(Option[ModelElement], Refers)] = {
      _refersTo.map(r => (r.toElement, r))
    }

  }

  trait LegacyOverrides {
    self: ElementModelImpl =>

    override def internalDirectOverrides: List[ModelElement] = {
      overrides.collect {
        case o if o.isDirect && o.toElement.isDefined => o.toElement.get
      }
    }

    override def internalTransitiveOverrides: List[ModelElement] = {
      overrides.collect {
        case o if o.toElement.isDefined => o.toElement.get
      }
    }

    override def allDirectOverrides: List[(Option[ModelElement], ElementId)] = {
      overrides.collect {
        case o if o.isDirect => (o.toElement, o.toElementId)
      }
    }

    override def allTransitiveOverrides: List[(Option[ModelElement], ElementId)] = {
      overrides.collect { case o => (o.toElement, o.toElementId) }
    }

    override def internalDirectOverriddenBy: List[ModelElement] = {
      overridden.collect {
        case o if o.isDirect => o.fromElement
      }
    }

    override def internalTransitiveOverriddenBy: List[ModelElement] = {
      overridden.map {
        _.fromElement
      }
    }

  }

  abstract sealed class ElementModelImpl(info: BasicElementInfo)
      extends ModelElement
      with LegacyReferences
      with LegacyOverrides {

    private def elementSort(e1: ElementModelImpl, e2: ElementModelImpl): Boolean = {
      (e1.compare(e2)) > 0
    }

    private def elementPositionSort(e1: ElementModelImpl, e2: ElementModelImpl): Boolean = {
      val p1 = e1.rawStart
      val p2 = e2.rawStart
      if (p1 == p2) elementSort(e1, e2)
      else p1 < p2
    }

    def complete(
        elementIdManager: ElementIdManager,
        modelElements: Map[ElementId, ElementModelImpl],
        relsFrom: BasicRelationshipInfo,
        relsTo: BasicRelationshipInfo
    ): Unit = {

      _within = (relsFrom.within
        .getOrElse(modelElementId, Nil)
        .map {
          _.toElement.get.asInstanceOf[ElementModelImpl]
        })
        .distinct
        .sortWith(elementSort)
      _children = (relsTo.within
        .getOrElse(modelElementId, Nil)
        .map {
          _.fromElement.asInstanceOf[ElementModelImpl]
        })
        .sortWith(elementPositionSort)
      _refersTo = relsFrom.refers.getOrElse(modelElementId, Nil)
      _refersFrom = relsTo.refers.getOrElse(modelElementId, Nil)
      _overrides = relsFrom.overrides.getOrElse(modelElementId, Nil)
      _overridden = relsTo.overrides.getOrElse(modelElementId, Nil)
    }

    override def extensions: Iterable[ExtensionData] = info.extensions

    val source: SourceData = info.source

    def project: Project = source.project

    def projects: ProjectSet = project.projects

    override val modelElementId = info.elementId

    override def name = modelElementId.debugValue

    override def flags: Long = info.flags

    //start set by `complete`
    private var _within: List[ElementModelImpl]   = _
    private var _children: List[ElementModelImpl] = _

    private[impl] var _refersTo: List[Refers]   = _
    private[impl] var _refersFrom: List[Refers] = _

    private var _overrides: List[Overrides]  = _
    private var _overridden: List[Overrides] = _
    //end set by `complete`

    override def overrides: List[Overrides]  = _overrides
    override def overridden: List[Overrides] = _overridden

    override def enclosing: List[ElementModelImpl] = _within

    override def classOrEnclosing: ClassLike = enclosing.head.classOrEnclosing

    // sorting in complete()  to make debugging easier
    override def allChildren: List[ElementModelImpl] = _children
    override def fields: List[FieldModel]            = _children.collect { case f: FieldModelImpl => f }

    override def methods: List[MethodModel] = _children.collect { case m: MethodModel => m }

    override def innerClassLike: Seq[ClassLike] = _children.collect { case c: ClassLikeModelImpl => c }

    final protected def typeName = this match {
      case _: ClassModelImpl        => "class"
      case _: ObjectModelImpl       => "object"
      case _: TraitModelImpl        => "trait"
      case _: ValModelImpl          => "val"
      case _: VarModelImpl          => "var"
      case _: PlainMethodModelImpl  => "def"
      case _: GetterMethodModelImpl => "def[getter]"
      case _: SetterMethodModelImpl => "def[setter]"
      case _: FieldsModelImpl       => "fields"
      case _: SourceModelImpl       => "source"
    }

    private val offsetStart      = info.startPos
    private val offsetEnd        = info.endPos
    private val offsetFocusStart = info.focusStart

    override protected def infoPosString: String = {
      s"$offsetStart-$offsetEnd"

      // TODO Disabled as we don't want to load the tree to work out line / column right now
      //    val pos = tree.pos
      //    s"${pos.startLine}:${pos.startColumn} - ${pos.endLine}:${pos.endColumn}"
      //
    }

    override def rawStart: Int = offsetStart

    override def rawEnd: Int = offsetEnd

    override def rawFocusStart: Int = offsetFocusStart

    override def sourceFileName: String = source.path.toString

    override def incomingReferences: Iterable[Refers] = _refersFrom

    override def outgoingReferences: Iterable[Refers] = _refersTo

    override def isAbstract: Boolean = false

    override def existsInSource: Boolean = offsetEnd != offsetStart
  }

  abstract sealed class ClassLikeModelImpl(info: BasicElementInfo)
      extends ElementModelImpl(info)
      with ClassLike {

    private var _extnds: List[Extends]     = _
    private var _extendedBy: List[Extends] = _

    override def complete(
        elementIdManager: ElementIdManager,
        modelElements: Map[ElementId, ElementModelImpl],
        relsFrom: BasicRelationshipInfo,
        relsTo: BasicRelationshipInfo
    ): Unit = {
      super.complete(elementIdManager, modelElements, relsFrom, relsTo)
      _extnds = relsFrom.extnds.getOrElse(modelElementId, Nil)
      _extendedBy = relsTo.extnds.getOrElse(modelElementId, Nil)

    }

    override protected def extnds: Seq[Extends] = _extnds

    override protected def extendedBy: Seq[Extends] = _extendedBy

    override def classOrEnclosing: ClassLike = this

    override def fullName: String = info.elementId.toString

    override def xtends[T](implicit cls: ClassTag[T]): Boolean = {
      xtends(PathNodes(ClassPath, cls.runtimeClass.getName))
    }

    override def xtends(symbol: ElementId): Boolean = {
      extnds.exists(_.toElementId == symbol)
    }

  }

  abstract sealed class FieldModelImpl(
      info: BasicElementInfo,
      val fieldName: String,
      override val isAbstract: Boolean,
      _fields: String
  ) extends ElementModelImpl(info)
      with FieldModel {

    override def fieldsInSameDeclaration =
      (declaredIn.map(_.fieldsInDeclaration)).getOrElse(Nil).asInstanceOf[List[fieldType]]

    override def complete(
        elementIdManager: ElementIdManager,
        modelElements: Map[ElementId, ElementModelImpl],
        relsFrom: BasicRelationshipInfo,
        relsTo: BasicRelationshipInfo
    ): Unit = {
      super.complete(elementIdManager, modelElements, relsFrom, relsTo)

      relsTo.getter.get(info.elementId) match {
        case None           => getter_ = None
        case Some(f :: Nil) => getter_ = Some(f.fromElement)
        case Some(error)    => ???
      }
      val fieldImpl = fields_ match {
        case "" => None
        case f: String =>
          Some(modelElements(elementIdManager(f)).asInstanceOf[FieldsModelImpl])
        case _ => ???
      }
      fieldImpl.foreach(_.addField(this))
      fields_ = fieldImpl
    }

    /**
     * before completion the name of the compound field
     * after completion Option[FieldsModel]
     */

    private var fields_ : AnyRef = _fields

    override def declaredIn = fields_.asInstanceOf[Option[FieldsModel]]

    private var getter_ : Option[GetterMethodModel] = _

    def getter                             = getter_
    def accessors: Iterable[AccessorModel] = getter

  }

  class ClassModelImpl(info: BasicElementInfo)
      extends ClassLikeModelImpl(info)
      with ClassModel

  class ObjectModelImpl(info: BasicElementInfo)
      extends ClassLikeModelImpl(info)
      with ObjectModel

  class TraitModelImpl(info: BasicElementInfo)
      extends ClassLikeModelImpl(info)
      with TraitModel

  class PlainMethodModelImpl(
      info: BasicElementInfo,
      val methodName: String,
      override val isAbstract: Boolean,
      val hasDeclaredType: Boolean
  ) extends ElementModelImpl(info)
      with PlainMethodModel

  class GetterMethodModelImpl(
      info: BasicElementInfo,
      val methodName: String,
      override val isAbstract: Boolean,
      val hasDeclaredType: Boolean
  ) extends ElementModelImpl(info)
      with GetterMethodModel {

    override def complete(
        elementIdManager: ElementIdManager,
        modelElements: Map[ElementId, ElementModelImpl],
        relsFrom: BasicRelationshipInfo,
        relsTo: BasicRelationshipInfo
    ): Unit = {
      super.complete(elementIdManager, modelElements, relsFrom, relsTo)
      relsFrom.getter.get(info.elementId) match {
        case None => field_ = None
        case Some(f :: Nil) =>
          field_ = f.toElement
        case Some(error) => ???
      }
    }

    private var field_ : Option[FieldModel] = _

    def field: Option[FieldModel] = field_
  }

  class SetterMethodModelImpl(
      info: BasicElementInfo,
      val methodName: String,
      override val isAbstract: Boolean,
      val hasDeclaredType: Boolean
  ) extends ElementModelImpl(info)
      with SetterMethodModel {

    override def complete(
        elementIdManager: ElementIdManager,
        modelElements: Map[ElementId, ElementModelImpl],
        relsFrom: BasicRelationshipInfo,
        relsTo: BasicRelationshipInfo
    ): Unit = {
      super.complete(elementIdManager, modelElements, relsFrom, relsTo)
      relsFrom.setter.get(info.elementId) match {
        case None => field_ = None
        case Some(f :: Nil) =>
          field_ = f.toElement
        case Some(error) => ???
      }
    }

    private var field_ : Option[VarModel] = _

    def field: Option[VarModel] = field_
  }

  class FieldsModelImpl(
      val info: BasicElementInfo,
      fieldName: String,
      isLazy: Boolean,
      size: Int
  ) extends ElementModelImpl(info)
      with FieldsModel {

    private var _fields = List.empty[FieldModel]

    def addField(impl: FieldModelImpl): Unit = {
      _fields ::= impl
    }

    override def fieldsInDeclaration: List[FieldModel] = _fields
  }

  class ValModelImpl(
      val info: BasicElementInfo,
      fieldName: String,
      isAbstract: Boolean,
      fields: String,
      val isLazy: Boolean
  ) extends FieldModelImpl(info, fieldName, isAbstract, fields)
      with ValModel {

    protected override def infoDetail = s"${super.infoDetail} lazy=$isLazy"

  }

  class VarModelImpl(
      val info: BasicElementInfo,
      fieldName: String,
      isAbstract: Boolean,
      fields: String
  ) extends FieldModelImpl(info, fieldName, isAbstract, fields)
      with VarModel {

    override def complete(
        elementIdManager: ElementIdManager,
        modelElements: Map[ElementId, ElementModelImpl],
        relsFrom: BasicRelationshipInfo,
        relsTo: BasicRelationshipInfo
    ): Unit = {
      super.complete(elementIdManager, modelElements, relsFrom, relsTo)
      relsTo.setter.get(info.elementId) match {
        case None => setter_ = None
        case Some(f :: Nil) =>
          setter_ = Some(f.fromElement)
        case Some(error) => ???
      }
    }

    private var setter_ : Option[SetterMethodModel] = _

    override def setter: Option[SetterMethodModel]  = setter_
    override def accessors: Iterable[AccessorModel] = getter ++ setter

  }

  class SourceModelImpl(val info: BasicElementInfo)
      extends ElementModelImpl(info)
      with SourceModel {

    def filename: Path = info.source.path

  }

}
