package scalaclean.model

import scalaclean.model.Mark.NoChange

object Mark {
  def dontChange[T <: SomeSpecificColour](reason: DontChange): Mark[T] =
    new NoChange[T](Set(reason))
  def specific[T <: SomeSpecificColour](specific: T) : Mark[T] =
    new Specific[T](specific)

  private object _initial extends Mark[SomeSpecificColour] {
    override def isInitial: Boolean = true

    override def toString: String = "Mark:initial"
  }
  def initial[T <: SomeSpecificColour] = _initial.asInstanceOf[Mark[_]].asInstanceOf[Mark[T]]
  private class Specific[T <: SomeSpecificColour](value: T) extends Mark[T] {
    override val specific: Option[T] = Some(value)
    override def toString: String = s"Mark:specific[${specific.get}]"
  }
  private class NoChange[T <: SomeSpecificColour](override val changeInhibitors: Set[DontChange]) extends Mark[T] {
    override def specific: Option[T] = None
    override def toString: String = s"Mark:no-change[$changeInhibitors]"
  }
  val maxChangeReasons = 1
}
sealed abstract class Mark[T <: SomeSpecificColour] {
  def isInitial: Boolean = false
  def specific: Option[T] = None
  def changesAreBanned = changeInhibitors.nonEmpty
  def changeInhibitors: Set[DontChange] = Set.empty
  type colour = T

  final def merge(other: Mark[T]): Mark[T] = {
    if (isInitial) other
    else if (changesAreBanned)
      if (changeInhibitors.size >= Mark.maxChangeReasons) this
      else if (other.changeInhibitors.subsetOf(this.changeInhibitors)) this
      else if (this.changeInhibitors.subsetOf(other.changeInhibitors)) other
      else new NoChange[T]((changeInhibitors ++ other.changeInhibitors).take(Mark.maxChangeReasons))
    else if (other.changesAreBanned) other
    else {
      //both are specific
      val thisS = specific.get
      val otherS = other.specific.get.asInstanceOf[thisS.RealType]
      val combined = (thisS merge otherS).asInstanceOf[T]
      if (combined eq thisS) this
      else if (combined eq otherS) other
      else new Mark.Specific[T](combined)
    }
  }

  final def banChange(reason: DontChange): Mark[T] = {
    if (changeInhibitors.size >= Mark.maxChangeReasons || changeInhibitors.contains(reason)) this
    else new NoChange[T](changeInhibitors + reason)
  }
  final def withSpecific(reason: T): Mark[T] = {
    if (changesAreBanned) this
    else new Mark.Specific(reason)
  }
}
trait SomeSpecificColour {
  type RealType <: SomeSpecificColour
  def merge(other: RealType): RealType
}
trait DontChange
case class SimpleReason(comment: String) extends DontChange
