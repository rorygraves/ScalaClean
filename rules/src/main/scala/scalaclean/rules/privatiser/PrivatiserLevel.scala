package scalaclean.rules.privatiser

import scalaclean.model.{Mark, ModelElement}
import scalaclean.util.SymbolUtils
import scalafix.v1.Symbol

private[privatiser] sealed trait PrivatiserLevel extends Mark {
  def keyword: Option[String] = None
  def reason: String
  def asText(context: ModelElement): Option[String]

  def combine(level: PrivatiserLevel): PrivatiserLevel
}

private[privatiser] case class Public(symbol: Symbol, reason:String) extends PrivatiserLevel {
  def combine(level: PrivatiserLevel): PrivatiserLevel = level

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case class NoChange(symbol: Symbol, reason: String) extends PrivatiserLevel {
  def combine(level: PrivatiserLevel): PrivatiserLevel = this
  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case class Undefined(symbol: Symbol) extends PrivatiserLevel {

  override def reason: String = "Initial"

  def combine(level: PrivatiserLevel): PrivatiserLevel = level
  override def asText(context: ModelElement): Option[String] = Some("/* cant detect usage !! */")
}

sealed abstract class Qualified extends PrivatiserLevel {
  def scope: Symbol
  protected def name: String

  override def asText(context: ModelElement): Option[String] = {
    context.enclosing.headOption match {
      case Some(enclosing) if enclosing.symbol == scope => Some(name)
      case _ => Some(s"$name[$scope]")
    }
  }

}
private[privatiser] case class Private(scope: Symbol, reason: String) extends Qualified {
  override val keyword = Some("private")

  def combine(level: PrivatiserLevel): PrivatiserLevel = {
    level match {
      case Private(otherScope, otherReason) =>
        val commonParent = SymbolUtils.findCommonParent(scope, otherScope)
        if (commonParent == scope) this
        else if (commonParent == otherScope) level
        else Private(commonParent, s"common parent of '$reason' and '$otherReason'")
      case _ => level
    }
  }

  override protected def name: String = "private"
}

private[privatiser] case class Protected(scope:Symbol, reason: String) extends Qualified {
  override val keyword = Some("protected")

  // TODO - actually implement combine for protected!
  def combine(level: PrivatiserLevel): PrivatiserLevel = this

  override protected def name: String = "protected"
}





