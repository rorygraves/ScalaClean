package scalaclean.model

import scalaclean.model.impl._

sealed trait Reference {
  def fromElementId: ElementId

  def toElementId: ElementId

  def fromElement: ModelElement

  def toElement: Option[ModelElement]

  final protected def refType: String = this match {
    case _: Refers => "Refers"
    case _: Extends => "Extends"
    case _: Overrides => "Overrides"
    case _: Within => "Within"
    case _: GetterImpl => "Getter"
    case _: SetterImpl => "Setter"
    case _: DuplicateImpl => "Duplicates"
    case _: ConstructorParamImpl => "ConstructorParam"
    case _: DefaultGetterImpl => "DefaultParameterGetter"
  }
}

trait Refers extends Reference {
  val isSynthetic: Boolean
}

trait Extends extends Reference {
  override def fromElement: ClassLike

  override def toElement: Option[ClassLike]

  val isDirect: Boolean
}

trait Overrides extends Reference {
  def isDirect: Boolean
}

trait Within extends Reference {}

package impl {

  sealed class ReferenceImpl[From <: ModelElement, To <: ModelElement](from: ElementId, to: ElementId) {
    private[this] var _from: AnyRef = from
    private[this] var _to: AnyRef   = to

    private[this] def modelSymbol(ref: AnyRef): ElementId = ref match {
      case s: ElementId    => s
      case m: ModelElement => m.modelElementId
    }

    def fromElement: From = _from.asInstanceOf[From]

    def toElement: Option[To] = _to match {
      case s: ElementId    => None
      case m: ModelElement => Some(m.asInstanceOf[To])
    }

    def fromElementId: ElementId = modelSymbol(_from)

    def toElementId: ElementId = modelSymbol(_to)

    def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
      _from = elements(fromElementId)
      elements.get(toElementId).foreach {
        _to = _
      }
    }

  }

  final class RefersImpl(from: ElementId, to: ElementId, val isSynthetic: Boolean)
      extends ReferenceImpl(from, to)
      with Refers

  final class ExtendsImpl(from: ElementId, to: ElementId, val isDirect: Boolean)
      extends ReferenceImpl[ClassLike, ClassLike](from, to)
      with Extends {

    override def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
      super.complete(elements)
      assert(fromElement.isInstanceOf[ClassLike], s"$fromElement is not a ClassLike")
      assert(toElement.forall(_.isInstanceOf[ClassLike]))
    }

  }

  final class OverridesImpl(from: ElementId, to: ElementId, val isDirect: Boolean)
      extends ReferenceImpl(from, to)
      with Overrides {

    override def toString: String = s"Overrides($from -> $to, isDirect = $isDirect"
  }

  final class GetterImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[GetterMethodModel, FieldModel](from, to)
      with Reference

  final class SetterImpl(from: ElementId, to: ElementId)
      extends ReferenceImpl[SetterMethodModel, VarModel](from, to)
      with Reference

  final class WithinImpl(from: ElementId, to: ElementId) extends ReferenceImpl(from, to) with Within {

    override def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
      super.complete(elements)
      assert(fromElement.isInstanceOf[ElementModelImpl])
      assert(toElement.isDefined, s"to reference $toElementId not found for $from")
    }

  }
  final class DuplicateImpl(from: ElementId, to: ElementId)
    extends ReferenceImpl[ElementModelImpl, ElementModelImpl](from, to)
    with Reference
  final class ConstructorParamImpl(from: ElementId, to: ElementId)
    extends ReferenceImpl[FieldModelImpl, FieldModelImpl](from, to)
      with Reference
  final class DefaultGetterImpl(from: ElementId, to: ElementId)
    extends ReferenceImpl[FieldModelImpl, PlainMethodModelImpl](from, to)
      with Reference

}
