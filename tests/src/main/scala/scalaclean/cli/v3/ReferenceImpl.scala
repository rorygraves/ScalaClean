package scalaclean.cli.v3

import scalaclean.model._
import scalafix.v1.Symbol

sealed class ReferenceImpl[From <: ModelElement, To<: ModelElement](  from: Symbol, to: Symbol) {

  private[this] var _from: AnyRef = from
  private[this] var _to: AnyRef = to

  private[this] def symbol(ref: AnyRef) = ref match {
    case s: Symbol => s
    case m: ModelElement => m.symbol
  }

  def fromElement = _from.asInstanceOf[From]
  def toElement = _to match {
    case s: Symbol => None
    case m: ModelElement => Some(m.asInstanceOf[To])
  }

  def fromSymbol = symbol(_from)
  def toSymbol = symbol(_to)

  def complete(elements: Map[Symbol, ElementModelImpl]) = {
    _from = elements(fromSymbol)
    elements.get(toSymbol) foreach {_to = _}
  }
}

final class RefersImpl(from:Symbol, to:Symbol, val isSynthetic: Boolean)
  extends ReferenceImpl(from,to) with Refers

final class ExtendsImpl(from:Symbol, to:Symbol, val isDirect: Boolean)
  extends ReferenceImpl[ClassLike, ClassLike](from,to) with Extends {

  override def complete(elements: Map[Symbol, ElementModelImpl]): Unit = {
    super.complete(elements)
    assert(fromElement.isInstanceOf[ClassLike])
    assert(toElement map (_.isInstanceOf[ClassLike]) getOrElse(true))
  }
}

final class OverridesImpl(from:Symbol, to:Symbol, val isDirect: Boolean)
  extends ReferenceImpl(from,to) with Overrides

final class WithinImpl(from:Symbol, to:Symbol) extends ReferenceImpl(from,to)
  with Within {
  override def complete(elements: Map[Symbol, ElementModelImpl]): Unit = {
    super.complete(elements)
    assert(fromElement.isInstanceOf[ElementModelImpl])
    assert(toElement.isDefined)
  }

}

