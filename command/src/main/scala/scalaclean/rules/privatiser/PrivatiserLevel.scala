package scalaclean.rules.privatiser

import scalaclean.model.{Mark, ElementId, ModelElement}
import scalaclean.util.SymbolUtils

private[privatiser] sealed trait PrivatiserLevel extends Mark {
  def shouldReplace(aModel: ModelElement): Boolean

  def reason: String

  def asText(context: ModelElement): Option[String]

  def widen(level: PrivatiserLevel): PrivatiserLevel
}

private[privatiser] case class Public(reason: String) extends PrivatiserLevel {
  override def shouldReplace(aModel: ModelElement) = true

  override def widen(level: PrivatiserLevel): PrivatiserLevel = this

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case class NoChange(reason: String) extends PrivatiserLevel {
  override def shouldReplace(aModel: ModelElement) = false

  override def widen(level: PrivatiserLevel): PrivatiserLevel = this

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case object Undefined extends PrivatiserLevel {

  override def reason: String = "Initial"

  override def shouldReplace(aModel: ModelElement) = false

  override def widen(level: PrivatiserLevel): PrivatiserLevel = level

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] object AccessScope {
  val None: AccessScope = AccessScope(ElementId.None, "")
}

private[privatiser] final case class AccessScope(elementId: ElementId, reason: String) {
  def print(name: String): String = s"$name $elementId $reason"

  def widen(other: AccessScope): AccessScope =
    if (elementId.isNone) other
    else if (other.elementId.isNone) this
    else AccessScope(SymbolUtils.findCommonParent(elementId, other.elementId), s"$reason AND ${other.reason}")
}

private[privatiser] object Scoped {
  def Private(scope: ElementId, reason: String) = Scoped(AccessScope(scope, reason), AccessScope.None, false)

  def Protected(scope: ElementId, reason: String, forceProtected: Boolean) = Scoped(AccessScope.None, AccessScope(scope, reason), forceProtected)
}

private[privatiser] final case class Scoped(privateScope: AccessScope, protectedScope: AccessScope, forceProtected: Boolean) extends PrivatiserLevel {
  def isProtected = {
    def commonParentScope: ElementId =
      if (protectedScope.elementId.isNone) privateScope.elementId
      else SymbolUtils.findCommonParent(protectedScope.elementId, privateScope.elementId)

    forceProtected || privateScope.elementId.isNone || commonParentScope != privateScope.elementId
  }

  def scope: ElementId = privateScope.elementId

  def scopeOrDefault(default: ElementId): ElementId = if (privateScope.elementId.isNone) default else privateScope.elementId

  override def asText(context: ModelElement): Option[String] = {
    val name = if (isProtected) "protected" else "private"
    context.enclosing.headOption match {
      case Some(enclosing) if scopeOrDefault(enclosing.modelElementId) == enclosing.modelElementId => Some(name)

      case _ =>
        if (privateScope.elementId.isRoot)
          Some(name)
        else
          Some(s"$name[${privateScope.elementId.innerScopeString}]")
    }
  }

  override def shouldReplace(aModel: ModelElement) = true

  override def toString = s"Scoped[${privateScope.print("private")} -- ${protectedScope.print("protected")}}"

  override def reason: String = {
    if (privateScope.elementId.isNone) protectedScope.reason
    else if (protectedScope.elementId.isNone) privateScope.reason
    else s"private due to (${privateScope.reason}), protected access from (${protectedScope.reason})"
  }

  def widen(level: PrivatiserLevel): PrivatiserLevel = level match {
    case p: Public => p
    case n: NoChange => n
    case Undefined => this
    case other: Scoped =>
      val privateWidened = this.privateScope.widen(other.privateScope)
      if (privateWidened.elementId.isRoot) Public(privateWidened.reason)
      else Scoped(privateWidened, protectedScope.widen(other.protectedScope), forceProtected || other.forceProtected)
  }
}
