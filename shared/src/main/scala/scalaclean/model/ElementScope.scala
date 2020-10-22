package scalaclean.model

import scala.annotation.tailrec

object ElementScope {

  @tailrec final def isOrHasParent(aChild: ElementId, aParent: ElementId): Boolean = {
    (aChild eq aParent) || (!aChild.isNone && !aChild.isRoot && isOrHasParent(aChild.parent, aParent))
  }

  def hasTransitiveParent(aChild: ElementId, aParent: ElementId) = {
    (aChild eq aParent) || (!aChild.isNone && isOrHasParent(aChild.parent, aParent))
  }

  final def hasParentScope(aChild: ElementId, aParent: ElementId): Boolean = {
    (!aChild.isNone && !aChild.isRoot && isOrHasParentScope(aChild.parent, aParent))
  }

  final def isOrHasParentScope(aChild: ElementId, aParent: ElementId): Boolean = {
    isOrHasParentScopeImpl(aChild.companionObjectOrSelf, aParent.companionObjectOrSelf)
  }

  @tailrec private def isOrHasParentScopeImpl(
      aChild: ElementId,
      aParent: ElementId
  ): Boolean = {
    (aChild == aParent) || (!aChild.isNone && !aChild.isRoot && isOrHasParentScopeImpl(
      aChild.parent,
      aParent
    ))
  }

  @scala.annotation.tailrec
  def findCommonScopeParent(manager: ElementIdManager,scope1: ElementId, scope2: ElementId): ElementId = {
    def depth(scope: ElementId): Int = {
      if (scope.isNone || scope.isRoot) 0 else depth(scope.parent) + 1
    }

    @scala.annotation.tailrec
    def parent(scope: ElementId, level: Int): ElementId = {
      if (level == 0) scope else parent(scope.parent, level - 1)
    }

    val depth1 = depth(scope1)
    val depth2 = depth(scope2)
    if (depth1 > depth2) {
      findCommonScopeParent(manager, parent(scope1, depth1 - depth2), scope2)
    } else if (depth2 > depth1) {
      findCommonScopeParent(manager, scope1, parent(scope2, depth2 - depth1))
    } else if (scope1.companionObjectOrSelf == scope2.companionObjectOrSelf) {
      if (scope1.isNone || scope1.isRoot) manager.Root
      else scope1
    } else findCommonScopeParent(manager, scope1.parent, scope2.parent)
  }

}
