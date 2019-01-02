package scalaclean.privatiser

import scalaclean.model.Colour
import scalafix.v1.Symbol

sealed trait PrivatiserLevel extends Colour {
  def reason: String
  def combine(level: PrivatiserLevelAdd): PrivatiserLevel
}

case object Initial extends PrivatiserLevel {
  def reason = "initial"
  override def combine(level: PrivatiserLevelAdd): PrivatiserLevel = level
}

sealed trait PrivatiserLevelAdd extends PrivatiserLevel

case class NoChange(reason: String) extends PrivatiserLevelAdd {
  override def combine(level: PrivatiserLevelAdd): PrivatiserLevel = this
}

case class Private(scope: Symbol, reason: String) extends PrivatiserLevelAdd with AnalyserUtils {
  override def combine(level: PrivatiserLevelAdd): PrivatiserLevel = {
    level match {
      case Private(otherScope, otherReason) =>
        val commonParent = findCommonParent(scope, otherScope)
        if (commonParent == scope) this
        else if (commonParent == otherScope) level
        else Private(commonParent, s"common parent of '$reason' and '$otherReason'")
      case _ => level
    }
  }
}

case class Protected(scope:Symbol, reason: String) extends PrivatiserLevelAdd {
  // TODO - actually implement combine for protected!
  override def combine(level: PrivatiserLevelAdd): PrivatiserLevel = this
}





