package scalaclean.model

sealed trait Reference extends Ordered[Reference] {
  def fromElementId: ElementId

  def toElementId: ElementId

  def fromElement: ModelElement

  def toElement: Option[ModelElement]

  protected def refType: String

  override def compare(that: Reference): Int = {
    var res = this.fromElementId.id compareTo that.fromElementId.id
    if (res == 0)
      res = this.toElementId.id compareTo that.toElementId.id
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

  sealed class ReferenceImpl[From <: ModelElement, To <: ModelElement](from: ElementId, to: ElementId) {
    private[this] var _from: AnyRef = from
    private[this] var _to: AnyRef = to

    private[this] def modelSymbol(ref: AnyRef): ElementId = ref match {
      case s: ElementId => s
      case m: ModelElement => m.elementId
    }

    def fromElement: From = _from.asInstanceOf[From]

    def toElement: Option[To] = _to match {
      case s: ElementId => None
      case m: ModelElement => Some(m.asInstanceOf[To])
    }

    def fromElementId: ElementId = modelSymbol(_from)

    def toElementId: ElementId = modelSymbol(_to)

    def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
      _from = elements(fromElementId)
      elements.get(toElementId) foreach {
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

    override def toString: String = s"Overrides($from -> $to, isDirect = $isDirect"
  }

  final class GetterImpl(from: ElementId, to: ElementId)
    extends ReferenceImpl[GetterMethodModel, FieldModel](from, to) with Overrides {
    override def isDirect: Boolean = true
  }

  final class SetterImpl(from: ElementId, to: ElementId)
    extends ReferenceImpl[SetterMethodModel, VarModel](from, to) with Overrides {
    override def isDirect: Boolean = true
  }

  final class WithinImpl(from: ElementId, to:ElementId)
    extends ReferenceImpl(from, to) with Within {
    override def complete(elements: Map[ElementId, ElementModelImpl]): Unit = {
      super.complete(elements)
      assert(fromElement.isInstanceOf[ElementModelImpl])
      assert(toElement.isDefined, s"to reference $toElementId not found for $from")
    }

  }


}

