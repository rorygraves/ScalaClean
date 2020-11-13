package scalaclean.rules.privatiser

import org.scalaclean.analysis.plugin.VisibilityData
import scalaclean.model._

private[privatiser] sealed trait PrivatiserLevel extends SomeSpecificColour {

  override type RealType = PrivatiserLevel

  override def merge(other: PrivatiserLevel): PrivatiserLevel = widen(other)

  def reason: String

  def asText(context: ModelElement, options: AbstractPrivatiserCommandLine): Option[String]

  def widen(level: PrivatiserLevel): PrivatiserLevel
}

private[privatiser] case class Public(reason: String) extends PrivatiserLevel {
  override def widen(level: PrivatiserLevel): PrivatiserLevel = this

  override def asText(context: ModelElement, options: AbstractPrivatiserCommandLine): Option[String] = None
}

private[privatiser] object AccessScope {
  val None: AccessScope = AccessScope(ElementIds.None, Set.empty)

  def apply(elementId: ElementId, reasons: Set[String]): AccessScope = {
    val scope = elementId.companionObjectOrSelf
    scope.pathType match {
      case ObjectPath | PackagePath | NonePath | ThisPath | RootPath =>
        new AccessScope(scope, reasons)
      case _ =>
        apply(elementId.parent, reasons)
    }
  }

}

private[privatiser] final case class AccessScope private (elementId: ElementId, reasons: Set[String]) {
  def print(name: String): String = s"$name $elementId $sortedReasons"
  def sortedReasons: List[String] = reasons.toList.sorted

  def widen(other: AccessScope): AccessScope =
    if (elementId.isNone) other
    else if (other.elementId.isNone) this
    else AccessScope(ElementScope.findCommonScopeParent(ElementIds, elementId, other.elementId), reasons ++ other.reasons)

}

private[privatiser] object Scoped {
  def Private(scope: ElementId, reason: String) = Scoped(AccessScope(scope, Set(reason)), AccessScope.None, false)

  def Protected(scope: ElementId, reason: String, forceProtected: Boolean) =
    Scoped(AccessScope.None, AccessScope(scope, Set(reason)), forceProtected)

}

private[privatiser] final case class Scoped(
    privateScope: AccessScope,
    protectedScope: AccessScope,
    forceProtected: Boolean
) extends PrivatiserLevel {

  def isPrivate = !isProtected

  def isProtected = {
    def commonParentScope: ElementId =
      if (protectedScope.elementId.isNone) privateScope.elementId
      else ElementScope.findCommonScopeParent(ElementIds, protectedScope.elementId, privateScope.elementId)

    forceProtected || privateScope.elementId.isNone || commonParentScope != privateScope.elementId
  }

  override def equals(x: Any) = x match {
    case that: Scoped =>
      this.isProtected == that.isProtected && this.scope == that.scope
    case _ => false
  }

  def scope: ElementId = (if (isProtected) protectedScope.elementId else privateScope.elementId).companionObjectOrSelf
//
//  def shouldChange(modelElement: ModelElement, options: AbstractPrivatiserCommandLine): Boolean = {
//    val currentVis = modelElement.extensionOfType[VisibilityData]
//    val (existingScope, explicitScope, existingProtected) =
//      currentVis match {
//        case None => (ElementId.Root, false, false)
//        case Some(VisibilityData(_, _, dec, aScope)) =>
//          (aScope.getOrElse(modelElement.modelElementId.parent), aScope.isDefined, dec == "protected")
//      }
//    val tighterScope = ElementScope.hasParentScope(scope, existingScope)
//    val sameScope    = scope == existingScope || scope.companionObjectOrSelf == existingScope
//    val isExplicitAndDoesntNeedToBe = explicitScope &&
//      (existingScope == modelElement.modelElementId.parent || existingScope.companionObjectOrSelf == modelElement.modelElementId.parent)
//    val moveToPrivate = existingProtected && !isProtected
//
//    tighterScope || (sameScope && moveToPrivate) || (isExplicitAndDoesntNeedToBe && isProtected == existingProtected)
//  }

  override def asText(context: ModelElement, options: AbstractPrivatiserCommandLine): Option[String] = {
    //private === private[implicitScopeClass]
    val implicitScopeClass: ClassLike = context match {
      case classLike: ClassLike if classLike.enclosing.head.isInstanceOf[SourceModel] => classLike
      case classLike: ClassLike                                                       => classLike.enclosing.head.classOrEnclosing
      case _                                                                          => context.classOrEnclosing
    }

    def enclosingScopeInNarrower: Boolean = context.enclosing.head match {
      case sourceModel: SourceModel => false
      case enclosing =>
        enclosing.classOrEnclosing.mark.asInstanceOf[Mark[PrivatiserLevel]].specific match {
          case Some(level) => this == this.widen(level)
          case _ => false
        }
    }

    def referencedInSubclass: Boolean = context match {
      case classLike: ClassLike => !classLike.extendedByElement().isEmpty
      case _                    => !context.overriddenByElement().isEmpty
    }
    //we dont need to label elements private if the enclosing class is private, or more restrictive
    //unless things inherit from this
    val canRemoveScope =         options.reduceDuplicateScopeChanges &&
      !referencedInSubclass &&
      this.isPrivate &&
      enclosingScopeInNarrower

    val implicitScope = implicitScopeClass.modelElementId.companionObjectOrSelf

    val (isDeclaredPublic, existingScope, explicitScope, existingProtected) =
      context.extensionOfType[VisibilityData] match {
        case None => (true, ElementIds.Root, false, false)
        case Some(VisibilityData(_, _, dec, aScope)) =>
          (false, aScope.getOrElse(implicitScope).companionObjectOrSelf, aScope.isDefined, dec == "protected")
      }
    val shouldRemoveScope = canRemoveScope && !isDeclaredPublic
    val tighterScope = !canRemoveScope && ElementScope.hasParentScope(scope, existingScope)
    val sameScope    = scope == existingScope
    val isExplicitAndDoesntNeedToBe =
      (explicitScope && existingScope == implicitScope)
    val moveToPrivate = existingProtected && !isProtected

    val shouldChange = shouldRemoveScope || tighterScope || (sameScope && moveToPrivate) || (isExplicitAndDoesntNeedToBe && isProtected == existingProtected)

    if (!shouldChange)
      None
    else {
      val name = if (isProtected) "protected" else "private"
      if (canRemoveScope)
        Some("")
      else if (scope == implicitScope) {
        // is the scope the same as it would be without the access the access qualifier
        // e.g {{{class X { private[X] def foo} }}} we can remove the "[X]"
        Some(name)
        //      } else {
        //        val scope = {
        //          val scope = privateScope.elementId
        //          if (
        //            !scope.isRoot && !scope.isNone && (scope == context.modelElementId || scope.companionOrSelf == context.modelElementId)
        //          )
        //            scope.parent
        //          else scope
        //        }
        //        if (scope.isRoot) Some(name)
      } else Some(s"$name[${scope.innerScopeString}]")
//      }
    }
  }

  override def toString = s"Scoped[${privateScope.print("private")} -- ${protectedScope.print("protected")}}"

  override def reason: String = {
    if (privateScope.elementId.isNone) protectedScope.sortedReasons.toString
    else if (protectedScope.elementId.isNone) privateScope.sortedReasons.toString
    else s"private due to (${privateScope.sortedReasons}), protected access from (${protectedScope.sortedReasons})"
  }

  def widen(level: PrivatiserLevel): PrivatiserLevel = level match {
    case p: Public   => p
    case other: Scoped =>
      val privateWidened = this.privateScope.widen(other.privateScope)
      if (privateWidened.elementId.isRoot) Public(privateWidened.sortedReasons.toString)
      else Scoped(privateWidened, protectedScope.widen(other.protectedScope), forceProtected || other.forceProtected)
  }

}
