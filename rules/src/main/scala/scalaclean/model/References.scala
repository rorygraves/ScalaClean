package scalaclean.model

import scalafix.v1.Symbol

sealed trait Reference {
  def fromSymbol: Symbol
  def toSymbol: Symbol

  def fromElement: ModelElement
  def toElement: Option[ModelElement]
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
  val isDirect: Boolean
}
trait Within extends Reference

