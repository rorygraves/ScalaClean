package scalaclean.model

sealed trait Reference extends Ordered[Reference] {
  def fromNewElementId: NewElementId

  def toNewElementId: NewElementId

  def fromElement: ModelElement

  def toElement: Option[ModelElement]

  protected def refType: String

  override def compare(that: Reference): Int = {
    var res = this.fromNewElementId.id compareTo that.fromNewElementId.id
    if (res == 0)
      res = this.toNewElementId.id compareTo that.toNewElementId.id
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

  sealed class ReferenceImpl[From <: ModelElement, To <: ModelElement](from: NewElementId, to: NewElementId) {
    private[this] var _from: AnyRef = from
    private[this] var _to: AnyRef = to

    private[this] def modelSymbol(ref: AnyRef): NewElementId = ref match {
      case s: NewElementId => s
      case m: ModelElement => m.modelElementId
    }

    def fromElement = _from.asInstanceOf[From]

    def toElement = _to match {
      case s: NewElementId => None
      case m: ModelElement => Some(m.asInstanceOf[To])
    }

//    def toElement2 = _to2 match {
//      case s:NewElementId => None
//      case m: ModelElement => Some(m.asInstanceOf[To])
//    }

    def fromNewElementId: NewElementId = modelSymbol(_from)

    def toNewElementId: NewElementId = modelSymbol(_to)

    def complete(elements: Map[NewElementId, ElementModelImpl]): Unit = {
      _from = elements(fromNewElementId)
      elements.get(toNewElementId) foreach {
        _to = _
      }
    }
  }

  final class RefersImpl(from: NewElementId, to: NewElementId, val isSynthetic: Boolean)
    extends ReferenceImpl(from, to) with Refers

  final class ExtendsImpl(from: NewElementId, to: NewElementId, val isDirect: Boolean)
    extends ReferenceImpl[ClassLike, ClassLike](from, to) with Extends {

    override def complete(elements: Map[NewElementId, ElementModelImpl]): Unit = {
      super.complete(elements)
//      assert(fromElement2.isInstanceOf[ClassLike], s"$fromElement is not a ClassLike")
//      assert(toElement2 map (_.isInstanceOf[ClassLike]) getOrElse (true))
      assert(fromElement.isInstanceOf[ClassLike], s"$fromElement is not a ClassLike")
      assert(toElement map (_.isInstanceOf[ClassLike]) getOrElse (true))
    }
  }

  final class OverridesImpl(from: NewElementId, to: NewElementId, val isDirect: Boolean)
    extends ReferenceImpl(from, to) with Overrides {

    override def toString: String = s"Overrides(${from} -> ${to}, isDirect = $isDirect"
  }

  final class GetterImpl(from: NewElementId, to: NewElementId)
    extends ReferenceImpl[GetterMethodModel, FieldModel](from, to) with Overrides {
    override def isDirect: Boolean = true
  }

  final class SetterImpl(from: NewElementId, to: NewElementId)
    extends ReferenceImpl[SetterMethodModel, VarModel](from, to) with Overrides {
    override def isDirect: Boolean = true
  }

  final class WithinImpl(from: NewElementId, to:NewElementId)
    extends ReferenceImpl(from, to) with Within {
    override def complete(elements: Map[NewElementId, ElementModelImpl]): Unit = {
      super.complete(elements)
//      assert(fromElement2.isInstanceOf[ElementModelImpl])
//      assert(toElement2.isDefined, s"to reference $toNewElementId not found for $from")
      assert(fromElement.isInstanceOf[ElementModelImpl])
      assert(toElement.isDefined, s"to reference $toNewElementId not found for $from")
    }

  }


}

