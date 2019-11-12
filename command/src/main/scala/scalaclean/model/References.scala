package scalaclean.model

import scalaclean.model.impl.ElementId
import scalafix.v1.Symbol

sealed trait Reference extends Ordered[Reference] {
  def fromSymbol: ElementId

  def toSymbol: ElementId

  def fromNewElementId: NewElementId

  def toNewElementId: NewElementId

  def fromElement: ModelElement

  def toElement: Option[ModelElement]

  protected def refType: String

  override def compare(that: Reference): Int = {
    var res = this.fromSymbol.symbol.value compareTo that.fromSymbol.symbol.value
    if (res == 0)
      res = this.toSymbol.symbol.value compareTo that.toSymbol.symbol.value
    if (res == 0)
      res = this.refType compareTo that.refType

    res
  }
}

trait Refers extends Reference {
  val isSynthetic: Boolean

  override protected def refType: String = "Refers"
}

trait Extends extends Reference {
  override def fromElement: ClassLike

  override def toElement: Option[ClassLike]

  val isDirect: Boolean

  override protected def refType: String = "Extends"
}

trait Overrides extends Reference {
  def isDirect: Boolean

  override protected def refType: String = "Overrides"
}

trait Within extends Reference {
  override protected def refType: String = "Within"

}
package impl {

  sealed class ReferenceImpl[From <: ModelElement, To <: ModelElement](from: ElementId, newFrom: NewElementId, to: ElementId, newTo: NewElementId) {

    private[this] var _from: AnyRef = from
    private[this] var _to: AnyRef = to

    private[this] var _from2: AnyRef = newFrom
    private[this] var _to2: AnyRef = newTo

    private[this] def symbol(ref: AnyRef): ElementId = ref match {
      case s: ElementId => s
      case m: ModelElement => m.symbol
    }

    private[this] def modelSymbol(ref: AnyRef): NewElementId = ref match {
      case s: NewElementId => s
      case m: ModelElement => m.modelElementId
    }

    def fromElement = _from.asInstanceOf[From]

    def fromElement2 = _from2.asInstanceOf[From]

    def toElement = _to match {
      case s: ElementId => None
      case m: ModelElement => Some(m.asInstanceOf[To])
    }

    def fromSymbol = symbol(_from)

    def toSymbol = symbol(_to)

    def fromNewElementId = modelSymbol(_from2)

    def toNewElementId = modelSymbol(_to2)

    def complete(elements: Map[ElementId, ElementModelImpl], modelElements: Map[NewElementId, ElementModelImpl]) = {
      _from = elements(fromSymbol)
      _from2 = modelElements(fromNewElementId)
      elements.get(toSymbol) foreach {
        _to = _
      }
      modelElements.get(toNewElementId) foreach {
        _to2 = _
      }

      //FIXME - old mode is broken
      //      assert (_from == _from2)
      //      if ( _to.isInstanceOf[ElementId] || _to2.isInstanceOf[NewElementId]) {
      //        assert(_to.isInstanceOf[ElementId] == _to2.isInstanceOf[NewElementId])
      //      } else {
      //        assert (to == _to2)
      //      }
    }
  }

  final class RefersImpl(from: ElementId, fromModel: NewElementId, to: ElementId, toModel: NewElementId, val isSynthetic: Boolean)
    extends ReferenceImpl(from, fromModel, to, toModel) with Refers

  final class ExtendsImpl(from: ElementId, fromModel: NewElementId, to: ElementId, toModel: NewElementId, val isDirect: Boolean)
    extends ReferenceImpl[ClassLike, ClassLike](from, fromModel, to, toModel) with Extends {

    if(fromModel.toString.contains("Local#16200") || toModel.toString.contains("Local#16200"))
      throw new IllegalStateException("AGGG")

    override def complete(elements: Map[ElementId, ElementModelImpl], modelElements: Map[NewElementId, ElementModelImpl]): Unit = {
      super.complete(elements, modelElements)
      assert(fromElement.isInstanceOf[ClassLike], s"$fromElement is not a ClassLike")
      assert(toElement map (_.isInstanceOf[ClassLike]) getOrElse (true))
    }
  }

  final class OverridesImpl(from: ElementId, fromModel: NewElementId, to: ElementId, toModel: NewElementId, val isDirect: Boolean)
    extends ReferenceImpl(from, fromModel, to, toModel) with Overrides {

    override def toString: String = s"Overrides(${from} -> ${to}, isDirect = $isDirect"
  }

  final class GetterImpl(from: ElementId, fromModel: NewElementId, to: ElementId, toModel: NewElementId)
    extends ReferenceImpl[GetterMethodModel, FieldModel](from, fromModel, to, toModel) with Overrides {
    override def isDirect: Boolean = true
  }

  final class SetterImpl(from: ElementId, fromModel: NewElementId, to: ElementId, toModel: NewElementId)
    extends ReferenceImpl[SetterMethodModel, VarModel](from, fromModel, to, toModel) with Overrides {
    override def isDirect: Boolean = true
  }

  final class WithinImpl(from: ElementId, fromModel: NewElementId, to: ElementId, toModel: NewElementId)
    extends ReferenceImpl(from, fromModel, to, toModel) with Within {
    override def complete(elements: Map[ElementId, ElementModelImpl], modelElements: Map[NewElementId, ElementModelImpl]): Unit = {
      super.complete(elements, modelElements)
      assert(fromElement.isInstanceOf[ElementModelImpl])
      assert(toElement.isDefined, s"to reference $toSymbol not found for $from")
    }

  }


}

