package scalaclean.privatiser

import scalaclean.model.Colour
import scalafix.v1.Symbol

private[privatiser] sealed trait PrivatiserLevel extends Colour {
  def keyword: Option[String] = None
  def reason: String
  def symbol: Symbol

  def combine(level: PrivatiserLevel): PrivatiserLevel
}

private[privatiser] case class Public(symbol: Symbol) extends PrivatiserLevel {
  def reason = "initial assumption: symbol should be public"
  def combine(level: PrivatiserLevel): PrivatiserLevel = level
}

private[privatiser] case class NoChange(symbol: Symbol, reason: String) extends PrivatiserLevel {
  def combine(level: PrivatiserLevel): PrivatiserLevel = this
}

private[privatiser] case class Private(symbol: Symbol, reason: String) extends PrivatiserLevel with AnalyserUtils {
  override val keyword = Some("private")

  def combine(level: PrivatiserLevel): PrivatiserLevel = {
    level match {
      case Private(otherScope, otherReason) =>
        val commonParent = findCommonParent(symbol, otherScope)
        if (commonParent == symbol) this
        else if (commonParent == otherScope) level
        else Private(commonParent, s"common parent of '$reason' and '$otherReason'")
      case _ => level
    }
  }
}

private[privatiser] case class Protected(symbol:Symbol, reason: String) extends PrivatiserLevel {
  override val keyword = Some("protected")

  // TODO - actually implement combine for protected!
  def combine(level: PrivatiserLevel): PrivatiserLevel = this
}





