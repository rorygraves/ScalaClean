package scalaclean.util

import scalaclean.model.impl.LegacyElementId
import scalafix.patch.Patch
import scalafix.v1._

import scala.meta.{Decl, Defn, Import, Pkg, Term, Tree}

object Scope {

  case class PkgScope(pkgName: String) extends Scope

  case class ObjScope(name: String) extends Scope

  case class ClassScope(name: String) extends Scope

  case class TraitScope(name: String) extends Scope

  case class MethodScope(name: String) extends Scope

  case class ValScope(name: String) extends Scope

  case class VarScope(name: String) extends Scope

}

trait Scope

object TreeVisitor {
  val continue: (Patch, Boolean) = (Patch.empty, true)
}

abstract class TreeVisitor()(implicit doc: SemanticDocument) {

  protected final def continue: (Patch, Boolean) = TreeVisitor.continue

  final def visitDocument(tree: Tree): Patch = {

    visitTree(tree, Nil)
  }

  private def visitChildren(t: Tree, scope: List[Scope]): Patch = {
    t.children.foldLeft(Patch.empty) { case (patch, child) =>
      patch + visitTree(child, scope)
    }
  }

  private def h(tree: Tree, handleRes: (Patch, Boolean), scope: List[Scope]): Patch = {
    val (patch, traverseChildren) = handleRes
    if (traverseChildren)
      patch + visitChildren(tree, scope)
    else
      patch
  }

  private def visitTree(t: Tree, scope: List[Scope]): Patch = {
    t match {
      case p: Pkg         => h(p, handlePackage(p.name, p, scope), Scope.PkgScope(p.name.toString()) :: scope)
      case o: Defn.Object => h(o, handleObject(LegacyElementId(o.symbol), o, scope), Scope.ObjScope(o.symbol.toString()) :: scope)
      case c: Defn.Class  => h(c, handleClass(LegacyElementId(c.symbol), c, scope), Scope.ClassScope(c.symbol.displayName) :: scope)
      case t: Defn.Trait  => h(t, handleTrait(LegacyElementId(t.symbol), t, scope), Scope.TraitScope(t.symbol.displayName) :: scope)
      case d: Defn.Def    => h(d, handleMethod(LegacyElementId(d.symbol), sig1(d), d, scope), Scope.MethodScope(sig1(d)) :: scope)
      case d: Decl.Def    => h(d, handleMethod(LegacyElementId(d.symbol), sig2(d), d, scope), Scope.MethodScope(sig2(d)) :: scope)
      case v: Defn.Val    => h(v, handleVal(v, scope), Scope.ValScope(v.symbol.displayName) :: scope)
      case v: Decl.Val    => h(v, handleVal(v, scope), Scope.ValScope(v.symbol.displayName) :: scope)
      case v: Defn.Var    => h(v, handleVar(v, scope), Scope.ValScope(v.symbol.displayName) :: scope)
      case v: Decl.Var    => h(v, handleVar(v, scope), Scope.ValScope(v.symbol.displayName) :: scope)
      case i: Import      => h(i, handleImport(i, scope), scope)
      case _              => h(t, handleOther(t, scope), scope)
    }
  }

  private def sig(t: Tree, params: List[Term.Param]) = s"${t.symbol}:${params.map(_.decltpe.get)}"
  private def sig1(x: Defn.Def)                      = sig(x, x.paramss.flatten)
  private def sig2(x: Decl.Def)                      = sig(x, x.paramss.flatten)

  def handleVar(varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean)

  def handleVar(varDef: Decl.Var, scope: List[Scope]): (Patch, Boolean)

  def handleVal(valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean)

  def handleVal(valDef: Decl.Val, scope: List[Scope]): (Patch, Boolean)

  def handleMethod(symbol: LegacyElementId, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean)

  def handleMethod(symbol: LegacyElementId, fullSig: String, method: Decl.Def, scope: List[Scope]): (Patch, Boolean)

  def handleObject(symbol: LegacyElementId, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean)

  def handleClass(symbol: LegacyElementId, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean)

  def handleTrait(symbol: LegacyElementId, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean)

  //non model entries
  def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean)

  def handlePackage(packageName: Term.Name, pkg: Pkg, scope: List[Scope]): (Patch, Boolean)

  def handleOther(tree: Tree, scope: List[Scope]): (Patch, Boolean)
}
