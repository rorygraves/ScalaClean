package scalaclean.rules.finaliser

import scalaclean.model.{Mark, ModelElement, SomeSpecificColour}

private[finaliser] sealed trait FinaliserLevel extends SomeSpecificColour {
  type RealType = FinaliserLevel
  def merge(other: RealType): RealType = widen(other)

  def reason: String
  def widen(finaliserLevel: FinaliserLevel): FinaliserLevel

  def asText(context: ModelElement): Option[String]
}

object Open {
  private val cache         = collection.mutable.Map[String, Open]()
  def apply(reason: String) = cache.getOrElseUpdate(reason, new Open(reason))
}

private[finaliser] case class Open private (reason: String) extends FinaliserLevel {

  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = finaliserLevel match {
    case Final       => this
    case _: Sealed   => this
    case x: Open     => Open(x.reason + ", " + reason)
  }

  override def asText(context: ModelElement): Option[String] = None
}

object Sealed {
  private val cache         = collection.mutable.Map[String, Sealed]()
  def apply(reason: String) = cache.getOrElseUpdate(reason, new Sealed(reason))
}

private[finaliser] case class Sealed private (reason: String) extends FinaliserLevel {

  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = finaliserLevel match {
    case Final       => this
    case _: Sealed   => this
    case x: Open     => x
  }

  override def asText(context: ModelElement): Option[String] = Some("sealed")
}

private[finaliser] case object Final extends FinaliserLevel {

  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = finaliserLevel match {
    case Final       => this
    case x: Sealed   => x
    case x: Open     => x
  }

  override def reason = "final"

  override def asText(context: ModelElement): Option[String] = Some("final")
}
