package scalaclean.model.impl

import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.Symbol

object SymbolCache {
  val cache = new ConcurrentHashMap[String, ElementId]()
  def apply(id:String) : ElementId = cache.computeIfAbsent(id, s => ElementId(Symbol(s)))
  def apply(id:Symbol) : ElementId = apply(id.value)
}
