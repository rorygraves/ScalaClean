package scalaclean.rules.privatiser

import scalaclean.model.impl.ElementId
import scalaclean.model.{Mark, ModelElement, Utils}
import scalaclean.util.SymbolUtils
import scalafix.patch.Patch

import scala.meta.Stat

private[privatiser] sealed trait PrivatiserLevel extends Mark {
  def shouldReplace(aModel: ModelElement): Boolean

  def reason: String

  def asText(context: ModelElement): Option[String]

  def widen(level: PrivatiserLevel): PrivatiserLevel

  def marker(stat: Stat): Patch = Patch.empty
}

private[privatiser] case class Public(reason: String) extends PrivatiserLevel {
  override def shouldReplace(aModel: ModelElement) = true

  def widen(level: PrivatiserLevel) = this

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case class NoChange(reason: String) extends PrivatiserLevel {
  override def shouldReplace(aModel: ModelElement) = false

  def widen(level: PrivatiserLevel) = this

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case object Undefined extends PrivatiserLevel {

  override def reason: String = "Initial"

  override def shouldReplace(aModel: ModelElement) = false

  def widen(level: PrivatiserLevel) = level

  override def asText(context: ModelElement): Option[String] = None

  override def marker(stat: Stat): Patch = Utils.addMarker(stat, "can't detect usage")
}

private[privatiser] object AccessScope {
  val None = AccessScope(ElementId.None, "")
}

private[privatiser] final case class AccessScope(symbol: ElementId, reason: String) {
  def print(name: String) = if (symbol.isNone) s"$name <not found>" else s"$name $symbol $reason"

  def widen(other: AccessScope) =
    if (symbol.isNone) other
    else if (other.symbol.isNone) this
    else AccessScope(SymbolUtils.findCommonParent(symbol, other.symbol), s"$reason AND ${other.reason}")
}

private[privatiser] object Scoped {
  def Private(scope: ElementId, reason: String) = Scoped(AccessScope(scope, reason), AccessScope.None, false)

  def Protected(scope: ElementId, reason: String, forceProtected: Boolean) = Scoped(AccessScope.None, AccessScope(scope, reason), forceProtected)
}

private[privatiser] final case class Scoped(privateScope: AccessScope, protectedScope: AccessScope, forceProtected: Boolean) extends PrivatiserLevel {
  def isProtected = {
    def commonParentScope =
      if (protectedScope.symbol.isNone) privateScope.symbol
      else SymbolUtils.findCommonParent(protectedScope.symbol, privateScope.symbol)

    forceProtected || privateScope.symbol.isNone || commonParentScope != privateScope.symbol
  }

  def scope: ElementId = privateScope.symbol

  def scopeOrDefault(default: ElementId): ElementId = privateScope.symbol.asNonEmpty.getOrElse(default)

  override def asText(context: ModelElement): Option[String] = {
    val name = if (isProtected) "protected" else "private"
    context.enclosing.headOption match {
      case Some(enclosing) if scopeOrDefault(enclosing.legacySymbol) == enclosing.legacySymbol => Some(name)

      case _ =>
        val scope = privateScope.symbol.displayName
        if (scope.isEmpty)
          Some(name)
        else
          Some(s"$name[${privateScope.symbol.displayName}]")
    }
  }

  override def shouldReplace(aModel: ModelElement) = true

  override def toString = s"Scoped[${privateScope.print("private")} -- ${protectedScope.print("protected")}}"

  override def reason: String = {
    if (privateScope.symbol.isNone) protectedScope.reason
    else if (protectedScope.symbol.isNone) privateScope.reason
    else s"private due to (${privateScope.reason}), protected access from (${protectedScope.reason})"
  }

  def widen(level: PrivatiserLevel): PrivatiserLevel = level match {
    case p: Public => p
    case n: NoChange => n
    case Undefined => this
    case other: Scoped =>
      val privateWidened = this.privateScope.widen(other.privateScope)
      if (privateWidened.symbol.isRootPackage) Public(privateWidened.reason)
      else Scoped(privateWidened, protectedScope.widen(other.protectedScope), forceProtected || other.forceProtected)
  }
}
