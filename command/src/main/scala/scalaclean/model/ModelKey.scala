package scalaclean.model

import scalafix.v1.Symbol

import scala.meta.inputs.Input

object ModelKey {
  def fromGlobal(sym: Symbol): ModelKey =
    if (sym.isGlobal) GlobalSymbolKey(sym)
    else throw new IllegalArgumentException(s"$sym is not global")


  def apply(sym: Symbol, input: Input): ModelKey = {
    if (sym.isGlobal) GlobalSymbolKey(sym)
    else if (sym.isLocal) input match {
      case Input.File(path, _) => LocalSymbolKey(sym, path.toString)
      case Input.VirtualFile(path, _) => LocalSymbolKey(sym, path.toString)
      case other => ???
    }
    else ???
  }

  case class GlobalSymbolKey(sym: Symbol) extends ModelKey {
    override def toCsv: String = s"G:$sym"
  }

 case class LocalSymbolKey(sym: Symbol, filename: String) extends ModelKey {
    override def toCsv: String = s"L:$filename::$sym"
  }

}
sealed trait ModelKey {
  def toCsv :String

}

