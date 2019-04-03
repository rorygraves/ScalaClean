package scalaclean.model.impl

import scalaclean.model.impl.v2.{ElementModelImpl, Projects}
import scalaclean.model.{Extends, ModelElement, Overrides, Refers, Within}
import scalafix.v1.Symbol

private[impl] sealed class ReferenceImpl(  from: Symbol, to: Symbol) {

  private[this] var _from: AnyRef = from
  private[this] var _to: AnyRef = to

  private[this] def symbol(ref: AnyRef) = ref match {
    case s: Symbol => s
    case m: ModelElement => m.symbol
  }
  private[this] def model(ref: AnyRef) = ref match {
    case s: Symbol => None
    case m: ModelElement => Some(m)
  }

  def fromElement = model(_from)
  def toElement = model(_from)

  def fromSymbol = symbol(_from)
  def toSymbol = symbol(_from)

  private[impl] def complete(elements: Map[Symbol, ElementModelImpl]) = {
    elements.get(fromSymbol) foreach {_from = _}
    elements.get(toSymbol) foreach {_to = _}
  }
}
private[impl] final class RefersImpl(from:Symbol, to:Symbol, val isSynthetic: Boolean) extends ReferenceImpl(from,to) with Refers
private[impl] final class ExtendsImpl(from:Symbol, to:Symbol, val isDirect: Boolean) extends ReferenceImpl(from,to) with Extends
/*private[impl]*/ final class OverridesImpl(from:Symbol, to:Symbol, val isDirect: Boolean) extends ReferenceImpl(from,to) with Overrides
private[impl] final class WithinImpl(from:Symbol, to:Symbol) extends ReferenceImpl(from,to) with Within

