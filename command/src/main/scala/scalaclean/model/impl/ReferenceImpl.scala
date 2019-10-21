package scalaclean.model.impl

import scalaclean.model._

sealed class ReferenceImpl[From <: ModelElement, To <: ModelElement](from: ElementId, to: ElementId) {

  private[this] var _from: AnyRef = from
  private[this] var _to: AnyRef = to

  private[this] def symbol(ref: AnyRef): ElementId = ref match {
    case ms: ElementId => ms
    case m: ModelElement => m.symbol
  }

  def fromElement: From = _from.asInstanceOf[From]

  def toElement: Option[To] = _to match {
    case ms: ElementId => None
    case m: ModelElement => Some(m.asInstanceOf[To])
  }

  def fromSymbol: ElementId = symbol(_from)

  def toSymbol: ElementId = symbol(_to)

  def complete(elements: Map[ElementId, ElementModelImpl]) = {
    _from = elements(fromSymbol)
    elements.get(toSymbol) foreach {
      _to = _
    }
  }
}

final class RefersImpl(from: ElementId, to: ElementId, val isSynthetic: Boolean)
  extends ReferenceImpl(from, to) with Refers

final class ExtendsImpl(from: ElementId, to: ElementId, val isDirect: Boolean)
  extends ReferenceImpl[ClassLike, ClassLike](from, to) with Extends {

  override def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
    super.complete(elements)
    assert(fromElement.isInstanceOf[ClassLike], s"$fromElement is not a ClassLike")
    assert(toElement map (_.isInstanceOf[ClassLike]) getOrElse (true))
  }
}

final class OverridesImpl(from: ElementId, to: ElementId, val isDirect: Boolean)
  extends ReferenceImpl(from, to) with Overrides {
  override def toString: String = s"Overrides(${from} -> ${to}, isDirect = $isDirect"
}

final class GetterImpl(from: ElementId, to: ElementId)
  extends ReferenceImpl[GetterMethodModel, FieldModel](from, to) with Overrides {
  override def isDirect: Boolean = true
}

final class SetterImpl(from: ElementId, to: ElementId)
  extends ReferenceImpl[SetterMethodModel, VarModel](from, to) with Overrides {
  override def isDirect: Boolean = true
}

final class WithinImpl(from: ElementId, to: ElementId) extends ReferenceImpl(from, to)
  with Within {
  override def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
    super.complete(elements)
    assert(fromElement.isInstanceOf[ElementModelImpl])
    assert(toElement.isDefined, s"to reference $toSymbol not found for $from")
  }

}

