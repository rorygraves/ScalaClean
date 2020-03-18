package scalaclean.util

import scalaclean.model.impl.LegacyElementId

object SymbolUtils {

  @scala.annotation.tailrec
  @deprecated //Not really for the ElementId
  def findCommonParent(scope1: LegacyElementId, scope2: LegacyElementId): LegacyElementId = {
    def depth(scope: LegacyElementId): Int = {
      if (scope.isNone) 0 else depth(scope.owner) + 1
    }

    @scala.annotation.tailrec
    def parent(scope: LegacyElementId, level: Int): LegacyElementId = {
      if (level == 0) scope else parent(scope.owner, level - 1)
    }

    val depth1 = depth(scope1)
    val depth2 = depth(scope2)
    if (depth1 > depth2) {
      findCommonParent(parent(scope1, depth1 - depth2), scope2)
    } else if (depth2 > depth1) {
      findCommonParent(scope1, parent(scope2, depth2 - depth1))
    } else if (scope1 == scope2) if (scope1.isNone) LegacyElementId.RootPackage else scope1
    else findCommonParent(scope1.owner, scope2.owner)
  }

}
