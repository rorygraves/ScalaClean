package scalaclean.model

import scalaclean.model.impl.ModelSymbol
import scalafix.v1.Symbol

sealed trait Reference extends Ordered[Reference] {
  def fromSymbol: ModelSymbol
  def toSymbol: ModelSymbol

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

