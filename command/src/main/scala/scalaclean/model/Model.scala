package scalaclean.model

import java.nio.file.Path

import org.scalaclean.analysis.{ AnnotationData, ExtensionData }

import scala.reflect.ClassTag
import scala.tools.nsc.symtab.Flags

sealed trait ModelElement extends Ordered[ModelElement] {

  override def compare(that: ModelElement): Int = modelElementId.id.compare(that.modelElementId.id)

  def modelElementId: ElementId

  var _mark: Mark[_] = null
  def mark           = _mark

  def mark_=[T <: SomeSpecificColour](newMark: Mark[T]): Unit = {
    if (_mark eq null)
      _mark = newMark
    else {
      assert(!newMark.isInitial)
      _mark = _mark.asInstanceOf[Mark[T]].merge(newMark)
    }
  }

  def name: String

  def flags: Long
  final def isFinal       = (Flags.FINAL & flags) != 0
  final def isPrivate     = (Flags.PRIVATE & flags) != 0
  final def isPrivateThis = (Flags.PrivateLocal & flags) == Flags.PrivateLocal

  /** an element unless its a source file */
  def enclosing: Option[ModelElement]
  def enclosingOf[T <: ModelElement: ClassTag: NotNothing]: Option[T]
  def enclosingIterator: Iterator[ModelElement]

  def classOrEnclosing: ClassLike

  def annotationsOf[T: ClassTag: NotNothing]: Iterator[AnnotationData] = {
    val name = implicitly[ClassTag[T]].runtimeClass.getName
    annotations.filter(_.fqName == name)
  }

  def annotations: Iterator[AnnotationData] = extensions.collect { case a: AnnotationData => a }

  def extensions: Iterator[ExtensionData]

  def extensionsOfType[T <: ExtensionData: ClassTag: NotNothing]: Iterator[T] = {
    extensions.collect { case a: T => a }
  }

  def extensionOfType[T <: ExtensionData: ClassTag: NotNothing]: Option[T] = {
    extensions.collectFirst { case a: T => a }
  }

  /**
   * There are certain times when the compiler model has multiple elements that, in ScalaCleans model are the same thing
   * so in that case Scalaclean will choose a primary element and some duplicates. All of the relationships are to the
   * primary, and the duplicates retain flags to indicate the alternate usages/meanings
   *
   * if a.duplicates(Set(b,c)) then b/c.duplicateOf(a)
   */
  def duplicateOf: Option[ModelElement]
  def duplicates: Set[_ <: ModelElement]

  /**
   * The elements that this element overrides. Note this is limited to those elements that are
   * compiled and have scalaclean metadata
   *
   * @see overridesElementId if you want the ones in libraries as well
   * @see overridesFull if you want full flexibility
   * @param direct if Some(true)  - only the direct overrides
   *              if Some(false) - exclude the direct overrides
   *              if None        - all
   * @param synthetic if Some(true)  - only the synthetic overrides
   *              if Some(false) - exclude the synthetic overrides
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def overridesElement(
      direct: Option[Boolean] = None,
      synthetic: Option[Boolean] = None,
      filter: Option[OverridesInternalReference => Boolean] = None
  ): Iterator[ModelElement]

  /**
   * The elements that this element overrides. Note this includes all elements
   *
   * @see overridesElement if you want just the compiled elements
   * @see overridesFull if you want full flexibility
   * @param direct if Some(true)  - only the direct overrides
   *              if Some(false) - exclude the direct overrides
   *              if None        - all
   * @param synthetic if Some(true)  - only the synthetic overrides
   *              if Some(false) - exclude the synthetic overrides
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def overridesElementId(
      direct: Option[Boolean] = None,
      synthetic: Option[Boolean] = None,
      filter: Option[OverridesReference => Boolean] = None
  ): Iterator[ElementId]

  /**
   * The classes and traits that this ClassLike overrides.
   *
   * @see overridesElement if you want just the compiled elements
   * @see overridesElementId if you want just the element ids
   * @param direct if Some(true)  - only the direct overrides
   *              if Some(false) - exclude the direct overrides
   *              if None        - all
   * @param synthetic if Some(true)  - only the synthetic overrides
   *              if Some(false) - exclude the synthetic overrides
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def overridesFull(
      direct: Option[Boolean] = None,
      synthetic: Option[Boolean] = None,
      filter: Option[OverridesReference => Boolean] = None
  ): Iterator[OverridesReference]

  /**
   * get the object, classes and traits that extend this
   *
   * @see overriddenByFull if you want all full flexiblity
   * @param direct if Some(true)  - only the direct overrides
   *              if Some(false) - exclude the direct overrides
   *              if None        - all
   * @param synthetic if Some(true)  - only the synthetic overrides
   *              if Some(false) - exclude the synthetic overrides
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def overriddenByElement(
      direct: Option[Boolean] = None,
      synthetic: Option[Boolean] = None,
      filter: Option[OverriddenByReference => Boolean] = None
  ): Iterator[ModelElement]

  /**
   * get the object, classes and traits that extend this
   *
   * @see overriddenByElement if you want just the elements
   * @param direct if Some(true)  - only the direct overrides
   *              if Some(false) - exclude the direct overrides
   *              if None        - all
   * @param synthetic if Some(true)  - only the synthetic overrides
   *              if Some(false) - exclude the synthetic overrides
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def overriddenByFull(
      direct: Option[Boolean] = None,
      synthetic: Option[Boolean] = None,
      filter: Option[OverriddenByReference => Boolean] = None
  ): Iterator[OverriddenByReference]

  def overridesExternal: Boolean


  /**
    * The elements that this element refers to. Note this is limited to those elements that are
    * compiled and have scalaclean metadata
    *
    * @see refersToElementId if you want the ones in libraries as well
    * @see refersToFull if you want full flexibility
    * @param synthetic if Some(true)  - only the synthetic references
    *              if Some(false) - exclude the synthetic references
    *              if None        - all
    * @param filter     a general purpose filter for the relations to consider
    */
  def refersToElement(
                       synthetic: Option[Boolean] = None,
                       filter: Option[RefersToInternalReference => Boolean] = None
                     ): Iterator[ModelElement]
  /**
    * The elements that this element refers to. Note this includes all elements
    *
    * @see refersToElement if you want just the compiled elements
    * @see refersToFull if you want full flexibility
    * @param synthetic if Some(true)  - only the synthetic references
    *              if Some(false) - exclude the synthetic references
    *              if None        - all
    * @param filter     a general purpose filter for the relations to consider
    */
  def refersToElementId(
                         synthetic: Option[Boolean] = None,
                         filter: Option[RefersToReference => Boolean] = None
                       ): Iterator[ElementId]

  /**
    * The classes and traits that this ClassLike overrides.
    *
    * @see refersToElement if you want just the compiled elements
    * @see refersToElementId if you want just the element ids
    * @param synthetic if Some(true)  - only the synthetic references
    *              if Some(false) - exclude the synthetic references
    *              if None        - all
    * @param filter     a general purpose filter for the relations to consider
    */
  def refersToFull(
                    synthetic: Option[Boolean] = None,
                    filter: Option[RefersToReference => Boolean] = None
                  ): Iterator[RefersToReference]

  /**
    * get the object, classes and traits that extend this
    *
    * @see referredToByFull if you want all full flexiblity
    * @param synthetic if Some(true)  - only the synthetic references
    *              if Some(false) - exclude the synthetic references
    *              if None        - all
    * @param filter     a general purpose filter for the relations to consider
    */
  def referredToByElement(
                           synthetic: Option[Boolean] = None,
                           filter: Option[ReferredToByReference => Boolean] = None
                         ): Iterator[ModelElement]

  /**
    * get the object, classes and traits that extend this
    *
    * @see referredToByElement if you want just the elements
    * @param synthetic if Some(true)  - only the synthetic references
    *              if Some(false) - exclude the synthetic references
    *              if None        - all
    * @param filter     a general purpose filter for the relations to consider
    */
  def referredToByFull(
                        synthetic: Option[Boolean] = None,
                        filter: Option[ReferredToByReference => Boolean] = None
                      ): Iterator[ReferredToByReference]

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

  def allChildren: List[ModelElement]

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

  def project: SingleProjectModel

  def flagsDebug = {
    //consider extending HasFlags

    type flag = {
      def flagsToString(value: Long): String
    }
    val f = Flags.asInstanceOf[flag]
    f.flagsToString(flags)
  }

  override def toString: String =
    s"$infoTypeName $infoName [$infoPosString] $flagsDebug $infoDetail [[$modelElementId]]"

}

sealed trait ClassLike extends ModelElement {

  def getCompanionObject(model: AllProjectsModel): Option[ObjectModel] = {
    val ele = modelElementId.companionObjectOrSelf
    if (ele == modelElementId) None
    else Some(model.element[ObjectModel](ele))
  }

  def fullName: String

  def xtends[T](implicit cls: ClassTag[T]): Boolean

  def xtends(symbol: ElementId): Boolean

  /**
   * The classes and traits that this ClassLike extends. Note this is limited to the classes and traits that are
   * compiled and have scalaclean metadata
   *
   * @see extendsElementId if you want the ones in libraries as well
   * @see extendsFull if you want full flexibility
   * @param direct if Some(true)  - only the direct extensions
   *              if Some(false) - exclude the direct extensions
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def extendsElement(
      direct: Option[Boolean] = None,
      filter: Option[ExtendsInternalReference => Boolean] = None
  ): Iterator[ClassLike]

  /**
   * The classes and traits that this ClassLike extends. Note this includes external libraries
   *
   * @see extendsElement if you want just the compiled elements
   * @see extendsFull if you want full flexibility
   * @param direct if Some(true)  - only the direct extensions
   *              if Some(false) - exclude the direct extensions
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def extendsElementId(
      direct: Option[Boolean] = None,
      filter: Option[ExtendsReference => Boolean] = None
  ): Iterator[ElementId]

  /**
   * The classes and traits that this ClassLike extends.
   *
   * @see extendsElement if you want just the compiled elements
   * @see extendsElementId if you want just the element ids
   * @param direct if Some(true)  - only the direct extensions
   *              if Some(false) - exclude the direct extensions
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def extendsFull(
      direct: Option[Boolean] = None,
      filter: Option[ExtendsReference => Boolean] = None
  ): Iterator[ExtendsReference]

  /**
   * get the object, classes and traits that extend this
   *
   * @see extendedByFull if you want all full flexiblity
   * @param direct if Some(true)  - only the direct extensions
   *              if Some(false) - exclude the direct extensions
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def extendedByElement(
      direct: Option[Boolean] = None,
      filter: Option[ExtendedByReference => Boolean] = None
  ): Iterator[ClassLike]

  /**
   * get the object, classes and traits that extend this
   *
   * @see extendedByElement if you want just the elements
   * @param direct if Some(true)  - only the direct extensions
   *              if Some(false) - exclude the direct extensions
   *              if None        - all
   * @param filter     a general purpose filter for the relations to consider
   */
  def extendedByFull(
      direct: Option[Boolean] = None,
      filter: Option[ExtendedByReference => Boolean] = None
  ): Iterator[ExtendedByReference]

  def selfType: Option[FieldModel]

}

sealed trait ClassModel extends ClassLike

sealed trait ObjectModel extends ClassLike with FieldModel {

  def isTopLevel: Boolean = {
    enclosing.forall(_.isInstanceOf[SourceModel])
  }

  override final def getter: Option[GetterMethodModel]                  = None
  override final def accessors: Iterable[AccessorModel]                 = Nil
  override final def declaredIn: Option[FieldsModel]                    = None
  override final def fieldsInSameDeclaration                            = Nil
  override final def isParameter: Boolean                               = false
  override final def associatedConstructorParameter: Option[FieldModel] = None
  override final def associatedClassLikeField: Option[FieldModel]       = None

  final type fieldType = ObjectModel
}

sealed trait TraitModel extends ClassLike

sealed trait MethodModel extends ModelElement {
  def methodName: String

  /**
   * is it an extra method added by scala clean
   * e.g. for the junction of 2 traits
   * tra
   */
  def scSynthetic: Boolean
}

sealed trait AccessorModel extends MethodModel with FieldOrAccessorModel {
  def field: Option[FieldModel]
}

sealed trait PlainMethodModel extends MethodModel {
  def defaultAccessorFor: Option[FieldModel]
}

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
  def selfTypeFor: Option[ClassLike]

  def isParameter: Boolean
  def associatedConstructorParameter: Option[FieldModel]
  def associatedClassLikeField: Option[FieldModel]
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

  def filename: Path
  def encoding: String
  def sourceLength: Int
  def sourceJavaHash: Int
  def sourceMurmurHash: Int
}

trait SingleProjectModel {

  /** Get the original source if it was retained (via -P:scalaclean-analysis-plugin:copySources:true in the compiler) */
  def originalSource(file: SourceModel): Option[String]
  def allProjects: AllProjectsModel
}

trait AllProjectsModel {

  def element[T <: ModelElement: ClassTag: NotNothing](id: ElementId): T

  def getElement[T <: ModelElement: ClassTag: NotNothing](id: ElementId): Option[T]

  def size: Int

  def allOf[T <: ModelElement: ClassTag: NotNothing]: Iterator[T]

  def printStructure(): Unit = allOf[ClassLike].foreach(cls => println(s"class ${cls.fullName}"))

  def projects: List[SingleProjectModel]
}

package impl {

  import java.nio.file.Path

  import org.scalaclean.analysis.FlagHelper

  import scala.collection.AbstractIterator

  import RelationshipNavigation._

  case class BasicElementInfo(
      project: ProjectImpl,
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
      setter: Map[ElementId, List[SetterImpl]],
      duplicate: Map[ElementId, List[DuplicateImpl]],
      ctorParam: Map[ElementId, List[ConstructorParamImpl]],
      defaultGetters: Map[ElementId, List[DefaultGetterImpl]],
      selfTypes: Map[ElementId, List[SelfTypeImpl]]
  ) {

    def sortValues: BasicRelationshipInfo = {
      import scala.concurrent.ExecutionContext.Implicits._
      import scala.concurrent.{ Await, Future }
      import scala.concurrent.duration.Duration
      val refersF = Future(refers.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) })
      val extndsF = Future(extnds.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) })
      val overridesF = Future(overrides.transform { case (k, v) =>
        v.sortBy(v => (v.fromElementId.id, v.toElementId.id))
      })
      val withinF = Future(within.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) })
      val getterF = Future(getter.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) })
      val setterF = Future(setter.transform { case (k, v) => v.sortBy(v => (v.fromElementId.id, v.toElementId.id)) })
      val duplicateF = Future(duplicate.transform { case (k, v) =>
        v.sortBy(v => (v.fromElementId.id, v.toElementId.id))
      })
      val ctorParamF = Future(ctorParam.transform { case (k, v) =>
        v.sortBy(v => (v.fromElementId.id, v.toElementId.id))
      })
      val defaultGettersF = Future(defaultGetters.transform { case (k, v) =>
        v.sortBy(v => (v.fromElementId.id, v.toElementId.id))
      })
      val selfTypeF = Future(selfTypes.transform { case (k, v) =>
        v.sortBy(v => (v.fromElementId.id, v.toElementId.id))
      })

      BasicRelationshipInfo(
        refers = Await.result(refersF, Duration.Inf),
        extnds = Await.result(extndsF, Duration.Inf),
        overrides = Await.result(overridesF, Duration.Inf),
        within = Await.result(withinF, Duration.Inf),
        getter = Await.result(getterF, Duration.Inf),
        setter = Await.result(setterF, Duration.Inf),
        duplicate = Await.result(duplicateF, Duration.Inf),
        ctorParam = Await.result(ctorParamF, Duration.Inf),
        defaultGetters = Await.result(defaultGettersF, Duration.Inf),
        selfTypes = Await.result(selfTypeF, Duration.Inf)
      )
    }

    def complete(modelElements: Map[ElementId, ElementModelImpl]): Unit = {
      refers.values.foreach(_.foreach(_.complete(modelElements)))
      extnds.values.foreach(_.foreach(_.complete(modelElements)))
      overrides.values.foreach(_.foreach(_.complete(modelElements)))
      within.values.foreach(_.foreach(_.complete(modelElements)))
      getter.values.foreach(_.foreach(_.complete(modelElements)))
      setter.values.foreach(_.foreach(_.complete(modelElements)))
      duplicate.values.foreach(_.foreach(_.complete(modelElements)))
      ctorParam.values.foreach(_.foreach(_.complete(modelElements)))
      defaultGetters.values.foreach(_.foreach(_.complete(modelElements)))
      selfTypes.values.foreach(_.foreach(_.complete(modelElements)))
    }

    def byTo: BasicRelationshipInfo = {
      def byToSymbol[T <: Reference](from: Map[ElementId, List[T]]): Map[ElementId, List[T]] = {
        from.values.flatten.groupBy(_.toElementId).map { case (k, v) => k -> v.toList }
      }

      BasicRelationshipInfo(
        refers = byToSymbol(refers),
        extnds = byToSymbol(extnds),
        overrides = byToSymbol(overrides),
        within = byToSymbol(within),
        getter = byToSymbol(getter),
        setter = byToSymbol(setter),
        duplicate = byToSymbol(duplicate),
        ctorParam = byToSymbol(ctorParam),
        defaultGetters = byToSymbol(defaultGetters),
        selfTypes = byToSymbol(selfTypes)
      )

    }

    def +(that: BasicRelationshipInfo): BasicRelationshipInfo = {
      val res = BasicRelationshipInfo(
        refers = this.refers ++ that.refers,
        extnds = this.extnds ++ that.extnds,
        overrides = this.overrides ++ that.overrides,
        within = this.within ++ that.within,
        getter = this.getter ++ that.getter,
        setter = this.setter ++ that.setter,
        duplicate = this.duplicate ++ that.duplicate,
        ctorParam = this.ctorParam ++ that.ctorParam,
        defaultGetters = this.defaultGetters ++ that.defaultGetters,
        selfTypes = this.selfTypes ++ that.selfTypes
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

  abstract sealed class ElementModelImpl(info: BasicElementInfo)
      extends ModelElement{

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

      _within = relsFrom.within.get(modelElementId) match {
        case Some(value :: Nil) =>
          value.toElement.getOrElse(
            throw new IllegalStateException(s"parent for $this (${value.toElementId}) not in model")
          )
        case None => assert(isInstanceOf[SourceModel]); null
        case Some(values) =>
          throw new IllegalStateException(s"${values.size} parents for $this ${values.map(_.toElementId)}")
      }
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

      relsFrom.duplicate.get(modelElementId) match {
        case None              =>
        case Some(List(value)) => _duplicateOf = value.toElement
        case _                 =>
      }
      relsTo.duplicate.get(modelElementId) match {
        case None         =>
        case Some(values) => _duplicates = values.map(_.fromElement).toSet
      }
    }

    override def extensions: Iterator[ExtensionData] = info.extensions.iterator

    val source: SourceData = info.source

    def project: ProjectImpl = source.project

    def projects: ProjectSet = project.projects

    override val modelElementId = info.elementId

    override def name = modelElementId.debugValue

    override def flags: Long = info.flags

    //start set by `complete`
    private[this] var _within: ElementModelImpl         = _
    private[this] var _children: List[ElementModelImpl] = _
    private def within = _within

    private[this] var _refersTo: List[RefersImpl]   = _
    private[this] var _refersFrom: List[RefersImpl] = _

    private[this] var _overrides: List[OverridesImpl]  = _
    private[this] var _overridden: List[OverridesImpl] = _

    private[this] var _duplicateOf = Option.empty[ElementModelImpl]
    private[this] var _duplicates  = Set.empty[ElementModelImpl]

    //end set by `complete`
    private def refers0[T](
                               rel: Seq[RefersImpl],
                               synthetic: Option[Boolean],
                               filterToDefined: Boolean,
                               filter: Option[RefersImpl => Boolean],
                               resultMapper: RefersImpl => T
                             ): Iterator[T] = {
      var it: Iterator[RefersImpl] = rel.iterator
      it = synthetic match {
        case Some(true)  => it.filter(_.isSynthetic)
        case Some(false) => it.filterNot(_.isSynthetic)
        case None        => it
      }
      if (filterToDefined)
        it = it.filter(_.toIsElement)
      it = filter match {
        case Some(f) =>
          it.filter(f)
        case None => it
      }
      it.map(resultMapper)
    }

    override def refersToElement(
                                   synthetic: Option[Boolean],
                                   filter: Option[RefersToInternalReference => Boolean]
                                 ): Iterator[ModelElement] = refers0(
      _refersTo,
      synthetic,
      true,
      filter.map(e => new RefersToInternalReferenceFilter(e)),
      _.toElementRaw
    )

    override def refersToElementId(
                                     synthetic: Option[Boolean],
                                     filter: Option[RefersToReference => Boolean]
                                   ): Iterator[ElementId] =
      refers0(_refersTo, synthetic, false, filter.map(e => new RefersToReferenceFilter(e)), _.toElementId)

    override def refersToFull(
                                synthetic: Option[Boolean],
                                filter: Option[RefersToReference => Boolean] = None
                              ): Iterator[RefersToReference] = refers0(
      _refersTo,
      synthetic,
      false,
      filter.map(e => new RefersToReferenceFilter(e)),
      NavigationData.to
    )

    override def referredToByElement(
                                      synthetic: Option[Boolean],
                                      filter: Option[ReferredToByReference => Boolean]
                                    ): Iterator[ModelElement] = refers0(
      _refersFrom,
      synthetic,
      false,
      filter.map(e => new ReferredToByReferenceFilter(e)),
      _.fromElement
    )

    override def referredToByFull(
                                   synthetic: Option[Boolean],
                                   filter: Option[ReferredToByReference => Boolean]
                                 ): Iterator[ReferredToByReference] = refers0(
      _refersFrom,
      synthetic,
      false,
      filter.map(e => new ReferredToByReferenceFilter(e)),
      NavigationData.from
    )

    private def overrides0[T](
        rel: Seq[OverridesImpl],
        direct: Option[Boolean],
        synthetic: Option[Boolean],
        filterToDefined: Boolean,
        filter: Option[OverridesImpl => Boolean],
        resultMapper: OverridesImpl => T
    ): Iterator[T] = {
      var it: Iterator[OverridesImpl] = rel.iterator
      it = direct match {
        case Some(true)  => it.filter(_.isDirect)
        case Some(false) => it.filterNot(_.isDirect)
        case None        => it
      }
      it = synthetic match {
        case Some(true)  => it.filter(_.isSynthetic)
        case Some(false) => it.filterNot(_.isSynthetic)
        case None        => it
      }
      if (filterToDefined)
        it = it.filter(_.toIsElement)
      it = filter match {
        case Some(f) =>
          it.filter(f)
        case None => it
      }
      it.map(resultMapper)
    }

    override def overridesElement(
        direct: Option[Boolean] = None,
        synthetic: Option[Boolean],
        filter: Option[OverridesInternalReference => Boolean] = None
    ): Iterator[ModelElement] = overrides0(
      _overrides,
      direct,
      synthetic,
      true,
      filter.map(e => new OverridesInternalReferenceFilter(e)),
      _.toElementRaw
    )

    override def overridesElementId(
        direct: Option[Boolean] = None,
        synthetic: Option[Boolean],
        filter: Option[OverridesReference => Boolean] = None
    ): Iterator[ElementId] =
      overrides0(_overrides, direct, synthetic, false, filter.map(e => new OverridesReferenceFilter(e)), _.toElementId)

    override def overridesFull(
        direct: Option[Boolean] = None,
        synthetic: Option[Boolean],
        filter: Option[OverridesReference => Boolean] = None
    ): Iterator[OverridesReference] = overrides0(
      _overrides,
      direct,
      synthetic,
      false,
      filter.map(e => new OverridesReferenceFilter(e)),
      NavigationData.to
    )

    override def overriddenByElement(
        direct: Option[Boolean],
        synthetic: Option[Boolean],
        filter: Option[OverriddenByReference => Boolean]
    ): Iterator[ModelElement] = overrides0(
      _overridden,
      direct,
      synthetic,
      false,
      filter.map(e => new OverriddenByReferenceFilter(e)),
      _.fromElement
    )

    override def overriddenByFull(
        direct: Option[Boolean],
        synthetic: Option[Boolean],
        filter: Option[OverriddenByReference => Boolean]
    ): Iterator[OverriddenByReference] = overrides0(
      _overridden,
      direct,
      synthetic,
      false,
      filter.map(e => new OverriddenByReferenceFilter(e)),
      NavigationData.from
    )

    override def overridesExternal: Boolean   = Caches.overridesExternal(this)

    override def enclosing: Option[ElementModelImpl] = Option(_within)

    override def enclosingOf[T <: ModelElement: ClassTag: NotNothing]: Option[T] =
      enclosingIterator.collectFirst { case res: T => res }

    override def enclosingIterator: Iterator[ModelElement] =
      if (_within eq null) Iterator.empty
      else
        new AbstractIterator[ModelElement] {
          var current                   = ElementModelImpl.this
          override def hasNext: Boolean = current.within ne null

          override def next(): ModelElement =
            if (!hasNext) Iterator.empty.next()
            else {
              current = current.within
              current
            }

        }

    override def classOrEnclosing: ClassLike = _within.classOrEnclosing

    override def duplicateOf: Option[ModelElement]  = _duplicateOf
    override def duplicates: Set[_ <: ModelElement] = _duplicates

    // sorting in complete()  to make debugging easier
    override def allChildren: List[ElementModelImpl] = _children
    override def fields: List[FieldModel]            = _children.collect { case f: FieldModelImpl => f }
    override def methods: List[MethodModel] = _children.collect { case m: MethodModel => m }
    override def innerClassLike: Seq[ClassLike] = _children.collect { case c: ClassLikeImpl => c }

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

    private def offsetStart      = info.startPos
    private def offsetEnd        = info.endPos
    private def offsetFocusStart = info.focusStart

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

    override def isAbstract: Boolean = false

    override def existsInSource: Boolean = offsetEnd != offsetStart
  }

  abstract sealed class ClassLikeImpl(info: BasicElementInfo) extends ElementModelImpl(info) with ClassLike {

    private[this] var _extnds: List[ExtendsImpl]        = _
    private[this] var _extendedBy: List[ExtendsImpl]    = _
    private var _selfType: Option[FieldModelImpl] = None

    override def complete(
        elementIdManager: ElementIdManager,
        modelElements: Map[ElementId, ElementModelImpl],
        relsFrom: BasicRelationshipInfo,
        relsTo: BasicRelationshipInfo
    ): Unit = {
      super.complete(elementIdManager, modelElements, relsFrom, relsTo)
      _extnds = relsFrom.extnds.getOrElse(modelElementId, Nil)
      _extendedBy = relsTo.extnds.getOrElse(modelElementId, Nil)
      relsFrom.selfTypes.get(info.elementId) match {
        case Some(List(selfTypeImpl)) =>
          val field = selfTypeImpl.toElement.get //its mandated to be in the model
          this._selfType = Some(field)
          field._selfTypeFor = Some(this)
        case None       =>
        case Some(list) => require(false, s"multiple self types for $this, $list")

      }
    }

    override def selfType: Option[FieldModelImpl] = _selfType

    override def classOrEnclosing: ClassLike = this

    override def fullName: String = info.elementId.toString

    override def xtends[T](implicit cls: ClassTag[T]): Boolean = {
      xtends(PathNodes(ClassPath, cls.runtimeClass.getName))
    }

    override def xtends(symbol: ElementId): Boolean = Caches.xtends(symbol, this)

    private def extendsClassLike0[T](
        rel: Seq[ExtendsImpl],
        direct: Option[Boolean],
        filterToDefined: Boolean,
        filter: Option[ExtendsImpl => Boolean],
        resultMapper: ExtendsImpl => T
    ): Iterator[T] = {
      var it: Iterator[ExtendsImpl] = rel.iterator
      it = direct match {
        case Some(true)  => it.filter(_.isDirect)
        case Some(false) => it.filterNot(_.isDirect)
        case None        => it
      }
      if (filterToDefined)
        it = it.filter(_.toIsElement)
      it = filter match {
        case Some(f) =>
          it.filter(f)
        case None => it
      }
      it.map(resultMapper)
    }

    override def extendsElement(
        direct: Option[Boolean] = None,
        filter: Option[ExtendsInternalReference => Boolean] = None
    ): Iterator[ClassLike] =
      extendsClassLike0(_extnds, direct, true, filter.map(e => new ExtendsInternalReferenceFilter(e)), _.toElementRaw)

    override def extendsElementId(
        direct: Option[Boolean] = None,
        filter: Option[ExtendsReference => Boolean] = None
    ): Iterator[ElementId] =
      extendsClassLike0(_extnds, direct, false, filter.map(e => new ExtendsReferenceFilter(e)), _.toElementId)

    override def extendsFull(
        direct: Option[Boolean] = None,
        filter: Option[ExtendsReference => Boolean] = None
    ): Iterator[ExtendsReference] =
      extendsClassLike0(_extnds, direct, false, filter.map(e => new ExtendsReferenceFilter(e)), NavigationData.to)

    override def extendedByElement(
        direct: Option[Boolean],
        filter: Option[ExtendedByReference => Boolean]
    ): Iterator[ClassLike] =
      extendsClassLike0(_extendedBy, direct, false, filter.map(e => new ExtendedByReferenceFilter(e)), _.fromElement)

    override def extendedByFull(
        direct: Option[Boolean],
        filter: Option[ExtendedByReference => Boolean]
    ): Iterator[ExtendedByReference] = extendsClassLike0(
      _extendedBy,
      direct,
      false,
      filter.map(e => new ExtendedByReferenceFilter(e)),
      NavigationData.from
    )

  }

  abstract sealed class FieldModelImpl(
      info: BasicElementInfo,
      val fieldName: String,
      override val isAbstract: Boolean,
      _fields: String
  ) extends ElementModelImpl(info)
      with FieldModel {
    private[impl] var _selfTypeFor: Option[ClassLikeImpl] = None

    override def selfTypeFor: Option[ClassLike] = _selfTypeFor

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

      relsFrom.ctorParam.get(info.elementId) match {
        case Some(ctor :: Nil) =>
          ctor.toElement match {
            case Some(ctorParam) =>
              ctorParam._classField = Some(this)
              this._ctorParam = Some(ctorParam)
            case None =>
          }

        case None        =>
        case Some(error) => throw new IllegalStateException(s"duplicate ctor params $error")
      }
      relsFrom.defaultGetters.get(info.elementId) match {
        case Some(ctor :: Nil) =>
          ctor.toElement match {
            case Some(ctorParam) =>
              ctorParam._defaultAccessorFor = Some(this)
              this._defaultAccessor = Some(ctorParam)
            case None =>
          }

        case None        =>
        case Some(error) => throw new IllegalStateException(s"duplicate ctor params $error")
      }
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

    override final def isParameter: Boolean = (info.flags & Flags.PARAM) != 0
    private var _classField                 = Option.empty[FieldModelImpl]
    private var _ctorParam                  = Option.empty[FieldModelImpl]
    private var _defaultAccessor            = Option.empty[PlainMethodModel]

    override final def associatedConstructorParameter: Option[FieldModel] = _ctorParam

    override final def associatedClassLikeField: Option[FieldModel] = _classField
  }

  class ClassModelImpl(info: BasicElementInfo) extends ClassLikeImpl(info) with ClassModel

  class ObjectModelImpl(info: BasicElementInfo) extends ClassLikeImpl(info) with ObjectModel {
    override def selfTypeFor: Option[ClassLike] = None
  }

  class TraitModelImpl(info: BasicElementInfo) extends ClassLikeImpl(info) with TraitModel

  sealed abstract class MethodModelImpl(
      info: BasicElementInfo,
      val methodName: String,
      override val isAbstract: Boolean,
      val hasDeclaredType: Boolean,
      val scSynthetic: Boolean
  ) extends ElementModelImpl(info)
      with MethodModel {}

  class PlainMethodModelImpl(
      info: BasicElementInfo,
      methodName: String,
      isAbstract: Boolean,
      hasDeclaredType: Boolean,
      scSynthetic: Boolean
  ) extends MethodModelImpl(info, methodName, isAbstract, hasDeclaredType, scSynthetic)
      with PlainMethodModel {
    var _defaultAccessorFor = Option.empty[FieldModelImpl]
    def defaultAccessorFor  = _defaultAccessorFor

  }

  class GetterMethodModelImpl(
      info: BasicElementInfo,
      methodName: String,
      isAbstract: Boolean,
      hasDeclaredType: Boolean,
      scSynthetic: Boolean
  ) extends MethodModelImpl(info, methodName, isAbstract, hasDeclaredType, scSynthetic)
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
      methodName: String,
      isAbstract: Boolean,
      hasDeclaredType: Boolean,
      scSynthetic: Boolean
  ) extends MethodModelImpl(info, methodName, isAbstract, hasDeclaredType, scSynthetic)
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

  class SourceModelImpl(
      val info: BasicElementInfo,
      val encoding: String,
      val sourceLength: Int,
      val sourceJavaHash: Int,
      val sourceMurmurHash: Int
  ) extends ElementModelImpl(info)
      with SourceModel {

    override def filename: Path = info.source.path

  }

}
