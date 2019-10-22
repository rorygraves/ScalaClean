package scalaclean.util

import scalaclean.model.impl.ElementId

object SymbolUtils {

  @scala.annotation.tailrec
  @deprecated //Not really for the ElementId
  def findCommonParent(scope1: ElementId, scope2: ElementId): ElementId = {
    def depth(scope: ElementId): Int = {
      if (scope.isNone) 0 else depth(scope.owner) +1
    }

    @scala.annotation.tailrec
    def parent(scope: ElementId, level: Int): ElementId = {
      if (level == 0) scope else parent(scope.owner, level - 1)
    }

    val depth1 = depth(scope1)
    val depth2 = depth(scope2)
    if (depth1 > depth2) {
      findCommonParent(parent(scope1, depth1 - depth2), scope2)
    } else if (depth2 > depth1) {
      findCommonParent(scope1, parent(scope2, depth2 - depth1))
    } else if (scope1 == scope2) if (scope1.isNone) ElementId.RootPackage else scope1
    else findCommonParent(scope1.owner, scope2.owner)
  }

}
