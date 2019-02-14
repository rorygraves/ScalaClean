package scalaclean.model

import scalafix.v1.Symbol

import scala.meta.Tree

case class RefersTo(tree: Tree, symbol: Symbol, isSynthetic: Boolean)