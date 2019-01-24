package scalaclean.rules.privatiser

import scalaclean.model.{Mark}
import scalaclean.util.SymbolUtils
import scalafix.v1.Symbol

private[privatiser] sealed trait PrivatiserLevel extends Mark {
  def keyword: Option[String] = None
  def reason: String
  def asText: Option[String]

  def combine(level: PrivatiserLevel): PrivatiserLevel
}

private[privatiser] case class Public(symbol: Symbol, reason:String) extends PrivatiserLevel {
  def combine(level: PrivatiserLevel): PrivatiserLevel = level

  override def asText: Option[String] = None
}

private[privatiser] case class NoChange(symbol: Symbol, reason: String) extends PrivatiserLevel {
  def combine(level: PrivatiserLevel): PrivatiserLevel = this
  override def asText: Option[String] = None
}

private[privatiser] case class Undefined(symbol: Symbol) extends PrivatiserLevel {

  override def reason: String = "Initial"

  def combine(level: PrivatiserLevel): PrivatiserLevel = level
  override def asText: Option[String] = ???
}

private[privatiser] case class Private(symbol: Symbol, reason: String) extends PrivatiserLevel with SymbolUtils {
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
  override def asText: Option[String] = Some(s"private[$symbol]")

}

private[privatiser] case class Protected(symbol:Symbol, reason: String) extends PrivatiserLevel {
  override val keyword = Some("protected")

  // TODO - actually implement combine for protected!
  def combine(level: PrivatiserLevel): PrivatiserLevel = this
  override def asText: Option[String] = Some(s"protected[$symbol]")
}





