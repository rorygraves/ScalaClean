package scalaclean.model.impl

import scalaclean.model.{
  ClassLike,
  ElementId,
  ExtendedByReference,
  ExtendsInternalReference,
  ExtendsReference,
  HasIsDirect,
  HasIsSynthetic,
  ModelElement,
  Reference
}

import scala.runtime.AbstractFunction1

private[impl] object RelationshipNavigation {

  abstract class RelsBaseFilter[T <: Reference] extends AbstractFunction1[T, Boolean] {
    protected var current: T = _

    override def apply(value: T): Boolean = {
      current = value
      applyImpl
    }

    def applyImpl: Boolean
  }

  trait RelsDirectFilter[T <: Reference with HasIsDirect] {
    self: RelsBaseFilter[T] =>
    def isDirect: Boolean = current.isDirect
  }

  trait RelsSyntheticFilter[T <: Reference with HasIsSynthetic] {
    self: RelsBaseFilter[T] =>
    def isSynthetic: Boolean = current.isSynthetic
  }

  trait RelsToFilter[From <: ModelElement, To <: ModelElement, Rels <: ReferenceImpl[From, To]] {
    self: RelsBaseFilter[Rels] =>
    def elementIsDefined: Boolean    = current.toIsElement
    def elementIfDefined: Option[To] = current.toElement
    def elementId: ElementId         = current.toElementId
  }

  trait RelsToInternalFilter[From <: ElementModelImpl, To <: ElementModelImpl, Rels <: ReferenceImpl[From, To]] {
    self: RelsBaseFilter[Rels] =>
    def element: To          = current.toElementRaw
    def elementId: ElementId = current.toElementId
  }

  trait RelsFromFilter[From <: ElementModelImpl, To <: ElementModelImpl, Rels <: ReferenceImpl[From, To]] {
    self: RelsBaseFilter[Rels] =>
    def element: From        = current.fromElement
    def elementId: ElementId = current.fromElementId
  }

  abstract class ExtendsReferenceBaseFilter extends RelsBaseFilter[ExtendsImpl] with RelsDirectFilter[ExtendsImpl]

  class ExtendsReferenceFilter(f: ExtendsReference => Boolean)
      extends ExtendsReferenceBaseFilter
      with RelsToFilter[ClassLikeImpl, ClassLikeImpl, ExtendsImpl]
      with ExtendsReference {
    override def applyImpl: Boolean = f(this)
  }

  class ExtendsInternalReferenceFilter(f: ExtendsInternalReference => Boolean)
      extends ExtendsReferenceBaseFilter
      with RelsToInternalFilter[ClassLikeImpl, ClassLikeImpl, ExtendsImpl]
      with ExtendsInternalReference {
    override def applyImpl: Boolean = f(this)
  }

  class ExtendedByReferenceFilter(f: ExtendedByReference => Boolean)
      extends ExtendsReferenceBaseFilter
      with RelsFromFilter[ClassLikeImpl, ClassLikeImpl, ExtendsImpl]
      with ExtendedByReference {
    override def applyImpl: Boolean = f(this)
  }

  object ExtendsToReferenceData {

    def apply(e: ExtendsImpl): ExtendsReference = new ExtendsReference {
      override def isDirect: Boolean = e.isDirect

      override def elementIsDefined: Boolean = e.toIsElement

      override def elementIfDefined: Option[ClassLike] = e.toElement

      override def elementId: ElementId = e.toElementId
    }

  }

  object ExtendedByReferenceData {

    def apply(e: ExtendsImpl): ExtendedByReference = new ExtendedByReference {
      override def isDirect: Boolean = e.isDirect

      override def element: ClassLike = e.fromElement
    }

  }

}
