package scalaclean.cli.v3

import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.Symbol

object SymbolCache {
  val cache = new ConcurrentHashMap[String, Symbol]()
  def apply(id:String) : Symbol = cache.computeIfAbsent(id, Symbol.apply)
  def apply(id:Symbol) : Symbol = apply(id.value)
}
