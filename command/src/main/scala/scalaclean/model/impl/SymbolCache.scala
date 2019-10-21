package scalaclean.model.impl

import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.Symbol

object SymbolCache {
  val cache = new ConcurrentHashMap[String, ModelSymbol]()
  def apply(id:String) : ModelSymbol = cache.computeIfAbsent(id, s => ModelSymbol(Symbol(s)))
  def apply(id:Symbol) : ModelSymbol = apply(id.value)
}
