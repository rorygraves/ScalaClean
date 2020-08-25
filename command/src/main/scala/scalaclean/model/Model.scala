package scalaclean.model

import org.scalaclean.analysis.{AnnotationData, ExtensionData}
import scalaclean.model.impl.ElementModelImpl

import scala.reflect.ClassTag

sealed trait ModelElement extends Ordered[ModelElement] {

  override def compare(that: ModelElement): Int = modelElementId.id.compare(that.modelElementId.id)

  def modelElementId: ElementId

  var mark: Mark = _

  def name: String

  def flags: Long

  //usually just one element. Can be >1 for  RHS of a val (a,b,c) = ...
  //where a,b,c are the enclosing
  def enclosing: List[ModelElement]

  def classOrEnclosing: ClassLike

  def annotationsOf(cls: Class[_]): Iterable[AnnotationData] =
    annotations filter (_.fqName == cls.getName)

  def annotations: Iterable[AnnotationData] =
    extensions collect {
      case a: AnnotationData => a
    }

  def extensions: Iterable[ExtensionData]

  def extensionsOfType[T <: ExtensionData: ClassTag]: Iterable[T] = {
    extensions collect {
      case a: T => a
    }
  }
  def extensionOfType[T <: ExtensionData: ClassTag]: Option[T] = {
    extensions collectFirst {
      case a: T => a
    }
  }

  //start target APIs
  // def outgoingReferences: Iterable[Refers] = allOutgoingReferences map (_._2)

  def overrides: Iterable[Overrides] = {
    val direct: Set[ElementId] = (allDirectOverrides map (_._2)).toSet
    allTransitiveOverrides map {
      case (_, modelSym) => new impl.OverridesImpl(modelElementId, modelSym, direct.contains(modelSym))
    }
  }

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

  final protected def infoTypeName = this match {
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
  def fullName: String

  def xtends[T](implicit cls: ClassTag[T]): Boolean

  def xtends(symbol: ElementId): Boolean

  def directExtends: Set[ElementId]

  def transitiveExtends: Set[ElementId]

  /** Dont use these */
  def directExtendedBy: Set[ClassLike]

  def transitiveExtendedBy: Set[ClassLike]
}

sealed trait ClassModel extends ClassLike

sealed trait ObjectModel extends ClassLike with FieldModel {
  def isTopLevel: Boolean = {
    enclosing.forall(_.isInstanceOf[SourceModel])
  }

  override final def getter: Option[GetterMethodModel] = None
  override final def accessors: Iterable[AccessorModel]  = Nil
  override final def declaredIn: Option[FieldsModel] = None
  override final def fieldsInSameDeclaration = Nil

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

  def allOf[T <: ModelElement : ClassTag]: Iterator[T]

  def printStructure(): Unit = allOf[ClassLike] foreach {
    cls => println(s"class ${cls.fullName}")
  }
}


package impl {

  import java.nio.file.Path

  import org.scalaclean.analysis.FlagHelper

  case class BasicElementInfo(
                               elementId: ElementId, source: SourceData,
                               startPos: Int, endPos: Int, focusStart: Int,
                               flags: Long, extensions: Seq[ExtensionData],
                               traversal: Int) {
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
                                    setter: Map[ElementId, List[SetterImpl]]) {
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
        from.values.flatten.groupBy(_.toElementId).map {
          case (k, v) => k -> v.toList
        }
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
      refersTo.collect {
        case r if r.toElement.isDefined => (r.toElement.get, r)
      }
    }

    override def internalIncomingReferences: List[(ModelElement, Refers)] = {
      refersFrom.collect {
        case r => (r.fromElement, r)
      }
    }

    override def allOutgoingReferences: List[(Option[ModelElement], Refers)] = {
      refersTo map {
        r => (r.toElement, r)
      }
    }
  }

  trait LegacyOverrides {
    self: ElementModelImpl =>

    override def internalDirectOverrides: List[ModelElement] = {
      overrides collect {
        case o if o.isDirect && o.toElement.isDefined => o.toElement.get
      }
    }

    override def internalTransitiveOverrides: List[ModelElement] = {
      overrides collect {
        case o if o.toElement.isDefined => o.toElement.get
      }
    }

    override def allDirectOverrides: List[(Option[ModelElement], ElementId)] = {
      overrides collect {
        case o if o.isDirect => (o.toElement, o.toElementId)
      }
    }

    override def allTransitiveOverrides: List[(Option[ModelElement], ElementId)] = {
      overrides collect {
        case o => (o.toElement, o.toElementId)
      }
    }

    override def internalDirectOverriddenBy: List[ModelElement] = {
      overridden collect {
        case o if o.isDirect => o.fromElement
      }
    }

    override def internalTransitiveOverriddenBy: List[ModelElement] = {
      overridden map {
        _.fromElement
      }
    }
  }

  trait LegacyExtends {
    self: ClassLikeModelImpl =>

    override def directExtends: Set[ElementId] = {
      extnds.collect {
        case s if s.isDirect => s.toElementId
      }.toSet
    }

    override def transitiveExtends: Set[ElementId] = {
      extnds.map(_.toElementId).toSet
    }

    override def directExtendedBy: Set[ClassLike] = {
      extendedBy.collect {
        case e if e.isDirect => e.fromElement
      }.toSet
    }

    override def transitiveExtendedBy: Set[ClassLike] = {
      extendedBy.map {
        _.fromElement
      }.toSet
    }

  }

  abstract sealed class ElementModelImpl(
    info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ModelElement
    with LegacyReferences with LegacyOverrides {
    def complete(
                  modelElements: Map[ElementId, ElementModelImpl],
                  relsFrom: BasicRelationshipInfo,
                  relsTo: BasicRelationshipInfo): Unit = {

      within = (relsFrom.within.getOrElse(modelElementId, Nil) map {
        _.toElement.get.asInstanceOf[ElementModelImpl]
      }).distinct
      children = relsTo.within.getOrElse(modelElementId, Nil) map {
        _.fromElement.asInstanceOf[ElementModelImpl]
      }
      refersTo = relsFrom.refers.getOrElse(modelElementId, Nil)
      refersFrom = relsTo.refers.getOrElse(modelElementId, Nil)
      _overrides = relsFrom.overrides.getOrElse(modelElementId, Nil)
      overridden = relsTo.overrides.getOrElse(modelElementId, Nil)
    }

    override def extensions: Iterable[ExtensionData] = info.extensions

    val source: SourceData = info.source

    def project: Project = source.project

    def projects: ProjectSet = project.projects

    override val modelElementId = info.elementId

    override def name = modelElementId.debugValue

    override def flags: Long = info.flags

    //start set by `complete`
    var within: List[ElementModelImpl] = _
    var children: List[ElementModelImpl] = _

    var refersTo: List[Refers] = _
    var refersFrom: List[Refers] = _

    var _overrides: List[Overrides] = _

    override def overrides: List[Overrides] = _overrides

    var overridden: List[Overrides] = _
    //end set by `complete`

    override def enclosing: List[ElementModelImpl] = within

    override def classOrEnclosing: ClassLike = enclosing.head.classOrEnclosing

    // sorting added to make debugging easier
    override def allChildren: List[ElementModelImpl] = children.sortBy(_.rawStart)
    override def fields: List[FieldModel] = children collect {
      case f: FieldModelImpl => f
    }

    override def methods: List[MethodModel] = children collect {
      case m: MethodModel => m
    }

    override def innerClassLike: Seq[ClassLike] = children collect {
      case c: ClassLikeModelImpl => c
    }

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

    private val offsetStart = info.startPos
    private val offsetEnd = info.endPos
    private val offsetFocusStart= info.focusStart

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

    override def incomingReferences: Iterable[Refers] = refersFrom

    override def outgoingReferences: Iterable[Refers] = refersTo

    override def isAbstract: Boolean = false

    override def existsInSource: Boolean = offsetEnd != offsetStart
  }

  abstract sealed class ClassLikeModelImpl(
    info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ElementModelImpl(info, relationships)
    with ClassLike with LegacyExtends {

    var extnds: List[Extends] = _
    var extendedBy: List[Extends] = _

    override def complete(
                           modelElements: Map[ElementId, ElementModelImpl],
                           relsFrom: BasicRelationshipInfo,
                           relsTo: BasicRelationshipInfo): Unit = {
      super.complete(modelElements, relsFrom, relsTo)
      extnds = relsFrom.extnds.getOrElse(modelElementId, Nil)
      extendedBy = relsTo.extnds.getOrElse(modelElementId, Nil)

    }

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
    info: BasicElementInfo, relationships: BasicRelationshipInfo,
    val fieldName: String, override val isAbstract: Boolean, _fields: String)
    extends ElementModelImpl(info, relationships) with FieldModel {
    override def fieldsInSameDeclaration = (declaredIn.map(_.fieldsInDeclaration)).getOrElse(Nil).asInstanceOf[List[fieldType]]

    override def complete(
                           modelElements: Map[ElementId, ElementModelImpl],
                           relsFrom: BasicRelationshipInfo,
                           relsTo: BasicRelationshipInfo): Unit = {
      super.complete(modelElements, relsFrom, relsTo)

      relsTo.getter.get(info.elementId) match {
        case None           => getter_ = None
        case Some(f :: Nil) => getter_ = Some(f.fromElement)
        case Some(error)    => ???
      }
      val fieldImpl = fields_ match {
        case "" => None
        case f: String =>
          Some(modelElements(ElementId(f)).asInstanceOf[FieldsModelImpl])
        case _ => ???
      }
      fieldImpl foreach (_.addField(this))
      fields_ = fieldImpl
    }

    /** before completion the name of the compound field
      * after completion Option[FieldsModel]
      */

    private var fields_ : AnyRef = _fields

    override def declaredIn = fields_.asInstanceOf[Option[FieldsModel]]

    private var getter_ : Option[GetterMethodModel] = _

    def getter = getter_
    def accessors: Iterable[AccessorModel] = getter

  }

  class ClassModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
    extends ClassLikeModelImpl(info, relationships) with ClassModel

  class ObjectModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
    extends ClassLikeModelImpl(info, relationships) with ObjectModel

  class TraitModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
    extends ClassLikeModelImpl(info, relationships) with TraitModel

  class PlainMethodModelImpl(
    info: BasicElementInfo, relationships: BasicRelationshipInfo,
    val methodName: String, override val isAbstract: Boolean, val hasDeclaredType: Boolean)
    extends ElementModelImpl(info, relationships) with PlainMethodModel

  class GetterMethodModelImpl(
    info: BasicElementInfo, relationships: BasicRelationshipInfo,
    val methodName: String, override val isAbstract: Boolean, val hasDeclaredType: Boolean)
    extends ElementModelImpl(info, relationships) with GetterMethodModel {

    override def complete(
                           modelElements: Map[ElementId, ElementModelImpl],
                           relsFrom: BasicRelationshipInfo,
                           relsTo: BasicRelationshipInfo): Unit = {
      super.complete(modelElements, relsFrom, relsTo)
      relsFrom.getter.get(info.elementId) match {
        case None => field_ =
          None
        case Some(f :: Nil) =>
          field_ = f.toElement
        case Some(error) => ???
      }
    }

    private var field_ : Option[FieldModel] = _

    def field: Option[FieldModel] = field_
  }

  class SetterMethodModelImpl(
    info: BasicElementInfo, relationships: BasicRelationshipInfo,
    val methodName: String, override val isAbstract: Boolean, val hasDeclaredType: Boolean)
    extends ElementModelImpl(info, relationships) with SetterMethodModel {

    override def complete(
                           modelElements: Map[ElementId, ElementModelImpl],
                           relsFrom: BasicRelationshipInfo,
                           relsTo: BasicRelationshipInfo): Unit = {
      super.complete(modelElements, relsFrom, relsTo)
      relsFrom.setter.get(info.elementId) match {
        case None => field_ =
          None
        case Some(f :: Nil) =>
          field_ = f.toElement
        case Some(error) => ???
      }
    }

    private var field_ : Option[VarModel] = _

    def field: Option[VarModel] = field_
  }

  class FieldsModelImpl(
    val info: BasicElementInfo, relationships: BasicRelationshipInfo,
    fieldName: String, isLazy: Boolean, size: Int
  ) extends ElementModelImpl(info, relationships) with FieldsModel {

    private var _fields = List.empty[FieldModel]

    def addField(impl: FieldModelImpl): Unit = {
      _fields ::= impl
    }

    override def fieldsInDeclaration: List[FieldModel] = _fields
  }

  class ValModelImpl(
    val info: BasicElementInfo, relationships: BasicRelationshipInfo,
    fieldName: String, isAbstract: Boolean, fields: String, val isLazy: Boolean)
    extends FieldModelImpl(info, relationships, fieldName, isAbstract, fields) with ValModel {

    protected override def infoDetail = s"${super.infoDetail} lazy=$isLazy"

  }

  class VarModelImpl(
    val info: BasicElementInfo, relationships: BasicRelationshipInfo,
    fieldName: String, isAbstract: Boolean, fields: String)
    extends FieldModelImpl(info, relationships, fieldName, isAbstract, fields) with VarModel {

    override def complete(
                           modelElements: Map[ElementId, ElementModelImpl],
                           relsFrom: BasicRelationshipInfo,
                           relsTo: BasicRelationshipInfo): Unit = {
      super.complete(modelElements, relsFrom, relsTo)
      relsTo.setter.get(info.elementId) match {
        case None => setter_ =
          None
        case Some(f :: Nil) =>
          setter_ = Some(f.fromElement)
        case Some(error) => ???
      }
    }

    private var setter_ : Option[SetterMethodModel] = _

    override def setter: Option[SetterMethodModel] = setter_
    override def accessors: Iterable[AccessorModel] = getter ++ setter

  }

  class SourceModelImpl(
    val info: BasicElementInfo,
    relationships: BasicRelationshipInfo) extends ElementModelImpl(info, relationships) with SourceModel {

    def filename: Path = info.source.path

  }
}
