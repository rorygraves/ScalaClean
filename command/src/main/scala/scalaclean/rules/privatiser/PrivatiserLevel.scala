package scalaclean.rules.privatiser

import org.scalaclean.analysis.plugin.VisibilityData
import scalaclean.model.{ElementId, ElementScope, Mark, ModelElement}

private[privatiser] sealed trait PrivatiserLevel extends Mark {
  def reason: String

  def asText(context: ModelElement): Option[String]

  def widen(level: PrivatiserLevel): PrivatiserLevel
}

private[privatiser] case class Public(reason: String) extends PrivatiserLevel {
  override def widen(level: PrivatiserLevel): PrivatiserLevel = this

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case class NoChange(reason: String) extends PrivatiserLevel {
  override def widen(level: PrivatiserLevel): PrivatiserLevel = this

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] case object Undefined extends PrivatiserLevel {

  override def reason = "Initial"

  override def widen(level: PrivatiserLevel): PrivatiserLevel = level

  override def asText(context: ModelElement): Option[String] = None
}

private[privatiser] object AccessScope {
  val None: AccessScope = AccessScope(ElementId.None, Set.empty)
}

private[privatiser] final case class AccessScope(elementId: ElementId, reasons: Set[String]) {
  def print(name: String): String = s"$name $elementId $sortedReasons"
  def sortedReasons = reasons.toList.sorted

  def widen(other: AccessScope): AccessScope =
    if (elementId.isNone) other
    else if (other.elementId.isNone) this
    else AccessScope(ElementScope.findCommonScopeParent(elementId, other.elementId), reasons ++ other.reasons)
}

private[privatiser] object Scoped {
  def Private(scope: ElementId, reason: String) = Scoped(AccessScope(scope, Set(reason)), AccessScope.None, false)

  def Protected(scope: ElementId, reason: String, forceProtected: Boolean) = Scoped(AccessScope.None, AccessScope(scope, Set(reason)), forceProtected)
}

private[privatiser] final case class Scoped(privateScope: AccessScope, protectedScope: AccessScope, forceProtected: Boolean) extends PrivatiserLevel {
  def isProtected = {
    def commonParentScope: ElementId =
      if (protectedScope.elementId.isNone) privateScope.elementId
      else ElementScope.findCommonScopeParent(protectedScope.elementId, privateScope.elementId)

    forceProtected || privateScope.elementId.isNone || commonParentScope != privateScope.elementId
  }

  def scope: ElementId = privateScope.elementId

  def scopeOrDefault(default: ElementId): ElementId = if (privateScope.elementId.isNone) default else privateScope.elementId

  def shouldChange(modelElement: ModelElement): Boolean = {
    val currentVis = modelElement.extensionOfType[VisibilityData]
    val (existingScope, explicitScope, existingProtected) =
      currentVis match {
        case None => (ElementId.Root, false, false)
        case Some(VisibilityData(_, _, dec, aScope)) => (aScope.getOrElse(modelElement.modelElementId.parent), aScope.isDefined, dec == "protected")
      }
    val tighterScope = ElementScope.hasParentScope(scope, existingScope)
    val sameScope = scope == existingScope || scope.companionOrSelf == existingScope
    val isExplicitAndDoesntNeedToBe = explicitScope &&
      (existingScope ==  modelElement.modelElementId.parent || existingScope.companionOrSelf ==  modelElement.modelElementId.parent)
    val moveToPrivate = existingProtected && !isProtected

    tighterScope || (sameScope && moveToPrivate) || (isExplicitAndDoesntNeedToBe && isProtected == existingProtected)
  }

  override def asText(context: ModelElement): Option[String] = {
    if (!shouldChange(context))
      None
    else {
      val name = if (isProtected) "protected" else "private"
      context.enclosing.headOption match {
        case Some(enclosing)
          if scopeOrDefault(enclosing.modelElementId) == enclosing.modelElementId
            || scopeOrDefault(enclosing.modelElementId) == enclosing.modelElementId.companionOrSelf =>
          Some(name)

        case _ =>
          val scope = {
            val scope = privateScope.elementId
            if (!scope.isRoot && !scope.isNone && (scope == context.modelElementId || scope.companionOrSelf == context.modelElementId))
              scope.parent else scope
          }
          if (scope.isRoot) Some(name)
          else Some(s"$name[${scope.innerScopeString}]")
      }
    }
  }

  override def toString = s"Scoped[${privateScope.print("private")} -- ${protectedScope.print("protected")}}"

  override def reason: String = {
    if (privateScope.elementId.isNone) protectedScope.sortedReasons.toString
    else if (protectedScope.elementId.isNone) privateScope.sortedReasons.toString
    else s"private due to (${privateScope.sortedReasons}), protected access from (${protectedScope.sortedReasons})"
  }

  def widen(level: PrivatiserLevel): PrivatiserLevel = level match {
    case p: Public => p
    case n: NoChange => n
    case Undefined => this
    case other: Scoped =>
      val privateWidened = this.privateScope.widen(other.privateScope)
      if (privateWidened.elementId.isRoot) Public(privateWidened.sortedReasons.toString)
      else Scoped(privateWidened, protectedScope.widen(other.protectedScope), forceProtected || other.forceProtected)
  }
}
