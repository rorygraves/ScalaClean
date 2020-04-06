package scalaclean.model.impl

import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.Tree

case class LegacyElementId private(isGlobal: Boolean, symbol: Symbol) {


  def value: String = symbol.value

  def debugValue: String = s"${if (isGlobal) "G" else "L"}:$value"

  def isRootPackage: Boolean = symbol.isRootPackage

  def displayName: String = symbol.displayName

  def asNonEmpty: Option[LegacyElementId] = symbol.asNonEmpty.map(s => LegacyElementId(s))

  @deprecated
  def owner: LegacyElementId = LegacyElementId(symbol.owner)

  def isNone: Boolean = symbol.isNone

}

object LegacyElementId {

  private val cache = new ConcurrentHashMap[String, LegacyElementId]()

  def apply(s: Symbol): LegacyElementId = {
    val strRep = s.value
    if (strRep.startsWith("G:"))
      throw new IllegalArgumentException("Boom")
    apply(strRep)
  }

  def apply(s: String): LegacyElementId = {

    if (s.startsWith("G:") || s.startsWith("L:")) {
      cache.computeIfAbsent(s, s => {
        val isGlobal = s.startsWith("G:")
        val symbol = Symbol(s.drop(2))
        LegacyElementId(isGlobal, symbol)
      })
    } else {
      val symbol = Symbol(s)
      if (symbol.isGlobal)
        apply("G:" + s)
      else
        apply("L:" + s)
    }
  }


  def fromTree(tree: Tree)(implicit doc: SemanticDocument): LegacyElementId = {
    import scalafix.v1.{Patch => _, _}
    LegacyElementId(tree.symbol)
  }

  val None: LegacyElementId = LegacyElementId(Symbol.None)

  val RootPackage: LegacyElementId = LegacyElementId(Symbol.RootPackage)
}
