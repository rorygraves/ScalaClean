package scalaclean.util

import scalafix.v1.Symbol

object SymbolUtils {

  def findCommonParent(scope1: Symbol, scope2: Symbol): Symbol = {
    def depth(scope: Symbol): Int = {
      if (scope.isNone) 0 else depth(scope.owner) +1
    }

    def parent(scope: Symbol, level: Int): Symbol = {
      if (level == 0) scope else parent(scope.owner, level - 1)
    }

    val depth1 = depth(scope1)
    val depth2 = depth(scope2)
    if (depth1 > depth2) {
      findCommonParent(parent(scope1, depth1 - depth2), scope2)
    } else if (depth2 > depth1) {
      findCommonParent(scope1, parent(scope2, depth2 - depth1))
    } else if (scope1 == scope2) if (scope1.isNone) Symbol.RootPackage else scope1
    else findCommonParent(scope1.owner, scope2.owner)
  }

}
