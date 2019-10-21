package scalaclean.model.impl

import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.Tree

case class ModelSymbol(symbol: Symbol) {
  def isGlobal: Boolean = symbol.isGlobal

  def value = symbol.value

  val debugValue: String = symbol.value

  def isRootPackage: Boolean = symbol.isRootPackage

  def displayName = symbol.displayName

  def asNonEmpty: Option[ModelSymbol] = symbol.asNonEmpty.map(s => ModelSymbol(s))

  def owner: ModelSymbol = ModelSymbol(symbol.owner)

  def isNone: Boolean = symbol.isNone

}

object ModelSymbol {

  def apply(s: String): ModelSymbol = ModelSymbol(Symbol(s))
  val AppObject: ModelSymbol = ModelSymbol(Symbol("G:scala/App#"))

  def fromTree(tree: Tree)(implicit doc: SemanticDocument): ModelSymbol = {
    import scalafix.v1.{Patch => _, _}
    ModelSymbol(tree.symbol)
  }

  val None: ModelSymbol = ModelSymbol(Symbol.None)

  val RootPackage: ModelSymbol = ModelSymbol(Symbol.RootPackage)
}
