package scalaclean.util

import scalaclean.model.Utils
import scalaclean.model.impl.ElementId
import scalafix.patch.Patch
import scalafix.v1._

import scala.meta.{Decl, Defn, Mod, Pat, Pkg, Stat, Term, Tree}

abstract class SymbolTreeVisitor()(implicit doc: SemanticDocument) extends TreeVisitor {

  protected def handlerSymbol(symbol: ElementId, mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean)

  protected def handlerPats(pats: Seq[Pat.Var], mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean)

  def handleVar(varDef: Defn.Var, scope: List[Scope]) = {
    handlerPats(Utils.readVars(varDef.pats), varDef.mods, varDef, scope)
  }

  override def handleVar(varDef: Decl.Var, scope: List[Scope]) = {
    handlerPats(Utils.readVars(varDef.pats), varDef.mods, varDef, scope)
  }

  override def handleVal(valDef: Decl.Val, scope: List[Scope]) = {
    handlerPats(Utils.readVars(valDef.pats), valDef.mods, valDef, scope)
  }

  override def handleVal(valDef: Defn.Val, scope: List[Scope]) = {
    handlerPats(Utils.readVars(valDef.pats), valDef.mods, valDef, scope)
  }

  override def handlePackage(packageName: Term.Name, pkg: Pkg, scope: List[Scope]): (Patch, Boolean) = {
    continue
  }

  override def handleMethod(symbol: ElementId, fullSig: String, method: Decl.Def, scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(symbol, method.mods, method, scope)
  }

  override def handleMethod(symbol: ElementId, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(symbol, method.mods, method, scope)
  }

  override def handleObject(objName: ElementId, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(objName, obj.mods, obj, scope)
  }

  override def handleClass(clsSymbol: ElementId, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(clsSymbol, cls.mods, cls, scope)
  }

  override def handleTrait(trtSymbol: ElementId, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean) = {
    handlerSymbol(trtSymbol, cls.mods, cls, scope)
  }

  override def handleOther(tree: Tree, scope: List[Scope]): (Patch, Boolean) = (Patch.empty, true)
}
