package scalaclean.model.impl

import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.Tree

case class ElementId private (isGlobal: Boolean, symbol: Symbol) {


  def value: String = symbol.value

  def debugValue: String = s"${if (isGlobal) "G" else "L"}:$value"

  def isRootPackage: Boolean = symbol.isRootPackage

  def displayName = symbol.displayName

  def asNonEmpty: Option[ElementId] = symbol.asNonEmpty.map(s => ElementId(s))

  def owner: ElementId = ElementId(symbol.owner)

  def isNone: Boolean = symbol.isNone

}

object ElementId {

  private val cache = new ConcurrentHashMap[String, ElementId]()

  def apply(s: Symbol): ElementId = {
    val strRep = s.value
    if(strRep.startsWith("G:"))
      throw new IllegalArgumentException("Boom")
    apply(strRep)
  }

  def apply(s: String): ElementId = {

    if(s.startsWith("G:") || s.startsWith("L:")) {
      cache.computeIfAbsent(s, s => {
        val isGlobal = s.startsWith("G:")
        val symbol = Symbol(s.drop(2))
        ElementId(isGlobal, symbol)
      })
    } else {
      val symbol =  Symbol(s)
      if(symbol.isGlobal)
        apply("G:" + s)
      else
        apply("L:" + s)
    }
  }

  val AppObject: ElementId = ElementId("G:scala/App#")

  def fromTree(tree: Tree)(implicit doc: SemanticDocument): ElementId = {
    import scalafix.v1.{Patch => _, _}
    ElementId(tree.symbol)
  }

  val None: ElementId = ElementId(Symbol.None)

  val RootPackage: ElementId = ElementId(Symbol.RootPackage)
}
