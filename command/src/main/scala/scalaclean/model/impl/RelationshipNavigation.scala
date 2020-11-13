package scalaclean.model.impl

import scalaclean.model.{ClassLike, ElementId, ExtendedByReference, ExtendsInternalReference, ExtendsReference, HasIsDirect, HasIsSynthetic, ModelElement, NotNothing, OverriddenByReference, OverridesInternalReference, OverridesReference, Reference}

import scala.runtime.AbstractFunction1

private[impl] object RelationshipNavigation {

  //generic relationship infra
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

  //extends relationship
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

  //overrides relationship
  abstract class OverridesReferenceBaseFilter
      extends RelsBaseFilter[OverridesImpl]
      with RelsDirectFilter[OverridesImpl]
      with RelsSyntheticFilter[OverridesImpl]

  class OverridesReferenceFilter(f: OverridesReference => Boolean)
      extends OverridesReferenceBaseFilter
      with RelsToFilter[ElementModelImpl, ElementModelImpl, OverridesImpl]
      with OverridesReference {
    override def applyImpl: Boolean = f(this)
  }

  class OverridesInternalReferenceFilter(f: OverridesInternalReference => Boolean)
      extends OverridesReferenceBaseFilter
      with RelsToInternalFilter[ElementModelImpl, ElementModelImpl, OverridesImpl]
      with OverridesInternalReference {
    override def applyImpl: Boolean = f(this)
  }

  class OverriddenByReferenceFilter(f: OverriddenByReference => Boolean)
      extends OverridesReferenceBaseFilter
      with RelsFromFilter[ElementModelImpl, ElementModelImpl, OverridesImpl]
      with OverriddenByReference {
    override def applyImpl: Boolean = f(this)
  }

  object NavigationData {
    abstract class RelsBase[T <: Reference](protected val data: T)

    trait RelsDirect[T <: Reference with HasIsDirect] {
      self: RelsBase[T] =>
      def isDirect: Boolean = data.isDirect
    }

    trait RelsSynthetic[T <: Reference with HasIsSynthetic] {
      self: RelsBase[T] =>
      def isSynthetic: Boolean = data.isSynthetic
    }

    trait RelsTo[From <: ModelElement, To <: ModelElement, Rels <: ReferenceImpl[From, To]] {
      self: RelsBase[Rels] =>
      def elementIsDefined: Boolean    = data.toIsElement
      def elementIfDefined: Option[To] = data.toElement
      def elementId: ElementId         = data.toElementId
    }

    trait RelsFrom[From <: ElementModelImpl, To <: ElementModelImpl, Rels <: ReferenceImpl[From, To]] {
      self: RelsBase[Rels] =>
      def element: From        = data.fromElement
      def elementId: ElementId = data.fromElementId
    }

    class ExtendsReferenceData(e: ExtendsImpl)
      extends RelsBase[ExtendsImpl](e)
        with RelsDirect[ExtendsImpl]
        with RelsTo[ClassLikeImpl, ClassLikeImpl, ExtendsImpl]
        with ExtendsReference

    class ExtendedByReferenceData(e: ExtendsImpl)
      extends RelsBase[ExtendsImpl](e)
        with RelsDirect[ExtendsImpl]
        with RelsFrom[ClassLikeImpl, ClassLikeImpl, ExtendsImpl]
        with ExtendedByReference

    def to(e: ExtendsImpl): ExtendsReference      = new ExtendsReferenceData(e)
    def from(e: ExtendsImpl): ExtendedByReference = new ExtendedByReferenceData(e)

    class OverridesReferenceData(e: OverridesImpl)
      extends RelsBase[OverridesImpl](e)
        with RelsSynthetic[OverridesImpl]
        with RelsDirect[OverridesImpl]
        with RelsTo[ElementModelImpl, ElementModelImpl, OverridesImpl]
        with OverridesReference

    class OverriddenByReferenceData(e: OverridesImpl)
      extends RelsBase[OverridesImpl](e)
        with RelsSynthetic[OverridesImpl]
        with RelsDirect[OverridesImpl]
        with RelsFrom[ElementModelImpl, ElementModelImpl, OverridesImpl]
        with OverriddenByReference

    def to(e: OverridesImpl): OverridesReference      = new OverridesReferenceData(e)
    def from(e: OverridesImpl): OverriddenByReference = new OverriddenByReferenceData(e)
  }

}
