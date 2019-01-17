package scalaclean.util

import scalafix.patch.Patch
import scalafix.v1._

import scala.meta.{Decl, Defn, Pkg, Stat, Term}

abstract class SymbolTreeVisitor()(implicit doc: SemanticDocument) extends TreeVisitor {

  protected def handlerSymbol(symbol: Symbol, stat: Stat, scope: List[Scope]): (Patch, Boolean)
  def handleVar(symbol: Symbol, varDef: Defn.Var,scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(symbol, varDef, scope)
  }


  override def handleVar(symbol: Symbol, varDef: Decl.Var, scope: List[Scope]): (Patch, Boolean) =  {
    handlerSymbol(symbol, varDef, scope)
  }

  override def handleVal(symbol: Symbol, valDef: Decl.Val, scope: List[Scope]): (Patch, Boolean) =  {
    handlerSymbol(symbol, valDef, scope)
  }

  def handleVal(symbol: Symbol, valDef: Defn.Val,scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(symbol, valDef, scope)
  }

  override def handlePackage(packageName: Term.Name, pkg: Pkg,scope: List[Scope]): (Patch, Boolean) = {
    (Patch.empty, true)
  }

  override def handleMethod(symbol: Symbol, fullSig: String, method: Decl.Def, scope: List[Scope]): (Patch, Boolean) =  {
    handlerSymbol(symbol, method, scope)
  }

  override def handleMethod(symbol: Symbol, fullSig: String, method: Defn.Def,scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(symbol, method, scope)
  }

  override def handleObject(objName: Symbol, obj: Defn.Object,scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(objName, obj, scope)
  }

  override def handleClass(clsSymbol: Symbol, cls: Defn.Class,scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(clsSymbol, cls, scope)
  }

  override def handleTrait(trtSymbol: Symbol, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(trtSymbol, cls, scope)
  }
}
