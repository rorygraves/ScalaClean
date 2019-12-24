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
    isOrHasParentScopeImpl(aChild, aParent, aParent.companionOrSelf) || {
      aChild.companionOrSelf != aChild && isOrHasParentScopeImpl(aChild.companionOrSelf, aParent, aParent.companionOrSelf)
    }
  }

  @tailrec private def isOrHasParentScopeImpl(aChild: ElementId, aParent: ElementId, parentCompanion: ElementId): Boolean = {
    (aChild eq aParent) || (aChild eq parentCompanion) || (!aChild.isNone && !aChild.isRoot && isOrHasParentScopeImpl(aChild.parent, aParent, parentCompanion))
  }

  @scala.annotation.tailrec
  def findCommonScopeParent(scope1: ElementId, scope2: ElementId): ElementId = {
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
      findCommonScopeParent(parent(scope1, depth1 - depth2), scope2)
    } else if (depth2 > depth1) {
      findCommonScopeParent(scope1, parent(scope2, depth2 - depth1))
    } else if (scope1 == scope2 || scope1 == scope2.companionOrSelf) {
      if (scope1.isNone || scope1.isRoot) ElementId.Root
      else scope1
    } else findCommonScopeParent(scope1.parent, scope2.parent)
  }



}
