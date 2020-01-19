package scalaclean.rules.finaliser

import scalaclean.model.{Mark, ModelElement}

private[finaliser] sealed trait FinaliserLevel extends Mark {
  def reason: String
  def widen(finaliserLevel: FinaliserLevel): FinaliserLevel

  def asText(context: ModelElement): Option[String]
}

private[finaliser] case class Open(reason: String) extends FinaliserLevel {

  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = this

  override def asText(context: ModelElement): Option[String] = None
}

private[finaliser] case class NoChange(reason: String) extends FinaliserLevel {
  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = this
  override def asText(context: ModelElement): Option[String] = None
}

private[finaliser] case object Undefined extends FinaliserLevel {
  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel = finaliserLevel

  override def reason = "Initial"

  override def asText(context: ModelElement): Option[String] = None
}
private[finaliser] case class Sealed(reason: String) extends FinaliserLevel {
  override def widen(finaliserLevel: FinaliserLevel): FinaliserLevel =
    finaliserLevel match {
      case Final => this
      case _: Sealed => this
      case x: Open => x
      case Undefined => ???
      case _: NoChange => ???
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
      case _: NoChange => ???
    }
  override def reason = "final"

  override def asText(context: ModelElement): Option[String] = Some("final")
}
