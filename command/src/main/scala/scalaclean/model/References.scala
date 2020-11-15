package scalaclean.model

import scalaclean.model.impl._

//TODO references should be removed from the API
//TODO classe should be moved to the impl package
sealed trait Reference {
  def fromElementId: ElementId

  def toElementId: ElementId

  def fromElement: ModelElement

  def toElement: Option[ModelElement]

  final protected def refType: String = this match {
    case _: Refers               => "Refers"
    case _: Extends              => "Extends"
    case _: Overrides            => "Overrides"
    case _: Within               => "Within"
    case _: GetterImpl           => "Getter"
    case _: SetterImpl           => "Setter"
    case _: DuplicateImpl        => "Duplicates"
    case _: ConstructorParamImpl => "ConstructorParam"
    case _: DefaultGetterImpl    => "DefaultParameterGetter"
    case _: SelfTypeImpl         => "SelfType"
  }

}

sealed trait InternalElementReference[T <: ModelElement] {
  def element: T
}

sealed trait ExternalElementReference[T <: ModelElement] {

  /** same as element.isDefined, but avoids the allocation */
  def elementIsDefined: Boolean
  def elementIfDefined: Option[T]
  def elementId: ElementId
}

sealed trait HasIsSynthetic { def isSynthetic: Boolean }

sealed trait HasIsDirect { def isDirect: Boolean }

sealed trait Refers extends Reference with HasIsSynthetic

sealed trait Extends extends Reference with HasIsDirect {
  override def fromElement: ClassLike

  override def toElement: Option[ClassLike]
}

sealed trait Overrides extends Reference with HasIsSynthetic with HasIsDirect {

  /**
   * special cases of the junction point between trait that have the same method, and the
   * classLike doesn't implement the method
   * e.g.
   * trait A { def x: Int}
   * trait B { def x = 1}
   * object C extends A with B
   *
   * if the relationship is synthetic then the source also is
   */
  def isSynthetic: Boolean
}

sealed trait Within extends Reference

//start concrete relationship results from model APIs

trait ExtendsReference         extends ExternalElementReference[ClassLike] with HasIsDirect
trait ExtendsInternalReference extends InternalElementReference[ClassLike] with HasIsDirect
trait ExtendedByReference      extends InternalElementReference[ClassLike] with HasIsDirect

trait OverridesReference         extends ExternalElementReference[ModelElement] with HasIsDirect with HasIsSynthetic
trait OverridesInternalReference extends InternalElementReference[ModelElement] with HasIsDirect with HasIsSynthetic
trait OverriddenByReference      extends InternalElementReference[ModelElement] with HasIsDirect with HasIsSynthetic

trait RefersToReference          extends ExternalElementReference[ModelElement] with HasIsSynthetic
trait RefersToInternalReference  extends InternalElementReference[ModelElement] with HasIsSynthetic
trait ReferredToByReference      extends InternalElementReference[ModelElement] with HasIsSynthetic
//end concrete relationship results from model APIs

package impl {

  sealed abstract class ReferenceImpl[From <: ModelElement: NotNothing, To <: ModelElement: NotNothing](
      from: ElementId,
      to: ElementId
  ) extends Reference {
    def clsFrom: Class[From]
    def clsTo: Class[To]

    private[this] var _from: AnyRef        = from
    private[this] var _to: AnyRef          = to
    private[impl] def toIsElement: Boolean = clsTo.isInstance(_to)

    private[this] def modelSymbol(ref: AnyRef): ElementId = ref match {
      case s: ElementId    => s
      case m: ModelElement => m.modelElementId
    }

    def fromElement: From = _from.asInstanceOf[From]

    def toElement: Option[To] = _to match {
      case s: ElementId    => None
      case _: ModelElement => Some(toElementRaw)
    }

    def toElementRaw = _to.asInstanceOf[To]

    def fromElementId: ElementId = modelSymbol(_from)

    def toElementId: ElementId = modelSymbol(_to)

    def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
      _from = clsFrom.cast(elements(fromElementId))
      elements.get(toElementId).foreach(e => _to = clsTo.cast(e))
    }

  }

  sealed abstract class ReferenceImplBase(from: ElementId, to: ElementId)
      extends ReferenceImpl[ElementModelImpl, ElementModelImpl](from, to) {
    override def clsFrom: Class[ElementModelImpl] = classOf[ElementModelImpl]

    override def clsTo: Class[ElementModelImpl] = classOf[ElementModelImpl]
  }

  final class RefersImpl(from: ElementId, to: ElementId, val isSynthetic: Boolean)
      extends ReferenceImplBase(from, to)
      with Refers

  final class ExtendsImpl(from: ElementId, to: ElementId, val isDirect: Boolean)
      extends ReferenceImpl[ClassLikeImpl, ClassLikeImpl](from, to)
      with Extends {

    override def clsFrom: Class[ClassLikeImpl] = classOf[ClassLikeImpl]

    override def clsTo: Class[ClassLikeImpl] = classOf[ClassLikeImpl]

  }

  final class OverridesImpl(from: ElementId, to: ElementId, val isDirect: Boolean, val isSynthetic: Boolean)
  //maybe should be Member, Member
      extends ReferenceImpl[ElementModelImpl, ElementModelImpl](from, to)
      with Overrides {

    override def clsFrom: Class[ElementModelImpl] = classOf[ElementModelImpl]

    override def clsTo: Class[ElementModelImpl] = classOf[ElementModelImpl]

    override def toString: String = s"Overrides($from -> $to, isDirect = $isDirect, isSynthetic = $isSynthetic"
  }

  final class GetterImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[GetterMethodModel, FieldModel](from, to)
      with Reference{

    override def clsFrom: Class[GetterMethodModel] = classOf[GetterMethodModel]

    override def clsTo: Class[FieldModel] = classOf[FieldModel]

  }

  final class SetterImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[SetterMethodModel, VarModel](from, to)
      with Reference {

    override def clsFrom: Class[SetterMethodModel] = classOf[SetterMethodModel]

    override def clsTo: Class[VarModel] = classOf[VarModel]

  }

  final class WithinImpl(from: ElementId, to: ElementId) extends ReferenceImpl[ElementModelImpl, ElementModelImpl](from, to) with Within {

    override def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
      super.complete(elements)
      assert(fromElement.isInstanceOf[SourceModel] != toElement.isDefined, s"reference $toElementId not found for $from")
    }

    override def clsFrom: Class[ElementModelImpl] = classOf[ElementModelImpl]

    override def clsTo: Class[ElementModelImpl] = classOf[ElementModelImpl]


  }

  final class DuplicateImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[ElementModelImpl, ElementModelImpl](from, to)
      with Reference {

    override def clsFrom: Class[ElementModelImpl] = classOf[ElementModelImpl]

    override def clsTo: Class[ElementModelImpl] = classOf[ElementModelImpl]

  }

  final class ConstructorParamImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[FieldModelImpl, FieldModelImpl](from, to)
      with Reference {

    override def clsFrom: Class[FieldModelImpl] = classOf[FieldModelImpl]

    override def clsTo: Class[FieldModelImpl] = classOf[FieldModelImpl]

  }

  final class DefaultGetterImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[FieldModelImpl, PlainMethodModelImpl](from, to)
      with Reference {

    override def clsFrom: Class[FieldModelImpl] = classOf[FieldModelImpl]

    override def clsTo: Class[PlainMethodModelImpl] = classOf[PlainMethodModelImpl]

  }

  final class SelfTypeImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[ClassLikeImpl, FieldModelImpl](from, to)
      with Reference {

    override def clsFrom: Class[ClassLikeImpl] = classOf[ClassLikeImpl]

    override def clsTo: Class[FieldModelImpl] = classOf[FieldModelImpl]

  }

}
