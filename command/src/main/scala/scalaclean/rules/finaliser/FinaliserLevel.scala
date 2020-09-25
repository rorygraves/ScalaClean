package scalaclean.rules.finaliser

import scalaclean.model.{Mark, ModelElement}

private[finaliser] sealed trait FinaliserLevel extends Mark {
  def reason: String
  def widen(finaliserLevel: FinaliserLevel): FinaliserLevel

  def asText(context: ModelElement): Option[String]
}
object Open {
  private val cache = collection.mutable.Map[String, Open]()
  def apply(reason: String) = cache.getOrElseUpdate(reason, new Open(reason))
}
private[finaliser] case class Open private(reason: String) extends FinaliserLevel {

  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = finaliserLevel match {
    case Final => this
    case _: Sealed => this
    case x: Open => Open(x.reason+", "+reason)
    case Undefined => ???
    case x: NoChange => x
  }

  override def asText(context: ModelElement): Option[String] = None
}

object NoChange {
  private val cache = collection.mutable.Map[String, NoChange]()
  def apply(reason: String) = cache.getOrElseUpdate(reason, new NoChange(reason))
}
private[finaliser] case class NoChange private(reason: String) extends FinaliserLevel {
  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = this
  override def asText(context: ModelElement): Option[String] = None
}

private[finaliser] case object Undefined extends FinaliserLevel {
  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = finaliserLevel

  override def reason = "Initial"

  override def asText(context: ModelElement): Option[String] = None
}
object Sealed {
  private val cache = collection.mutable.Map[String, Sealed]()
  def apply(reason: String) = cache.getOrElseUpdate(reason, new Sealed(reason))
}
private[finaliser] case class Sealed private (reason: String) extends FinaliserLevel {
  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel =
    finaliserLevel match {
      case Final => this
      case _: Sealed => this
      case x: Open => x
      case Undefined => ???
      case x: NoChange => x
    }

  override def asText(context: ModelElement): Option[String] = Some("sealed")
}
private[finaliser] case object Final extends FinaliserLevel {

  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel =
    finaliserLevel match {
      case Final => this
      case x: Sealed => x
      case x: Open => x
      case Undefined => ???
      case x: NoChange => x
    }
  override def reason = "final"

  override def asText(context: ModelElement): Option[String] = Some("final")
}
