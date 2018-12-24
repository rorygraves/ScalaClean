package scalaclean.util

import scalafix.patch.Patch
import scalafix.v1._

import scala.meta.{Defn, Pkg, Term, Tree}

object Scope {
  case class PkgScope(pkgName: String) extends Scope
  case class ObjScope(name :String) extends Scope
  case class ClassScope(name : String) extends Scope
  case class MethodScope(name : String) extends Scope
  case class ValScope(name : String) extends Scope
  case class VarScope(name : String) extends Scope
}

trait Scope

abstract class TreeVisitor()(implicit doc: SemanticDocument) {

  final def visitDocument(tree: Tree): Patch = {

    visitTree(tree, Nil)
  }

  private def visitChildren(t: Tree, scope: List[Scope]): Patch = {
    t.children.foldLeft(Patch.empty) { case (patch, child) =>
      patch + visitTree(child, scope)
    }
  }

  private def processHandler(tree: Tree, handleRes: (Patch, Boolean), scope: List[Scope]): Patch = {
    val (patch, traverseChildren) = handleRes
    if (traverseChildren)
      patch + visitChildren(tree, scope)
      else
    patch
  }


  def visitTree(tree: Tree,scope: List[Scope]): Patch = {

    tree match {
      case pkg: Pkg =>
        val newScope = Scope.PkgScope(pkg.name.toString()) :: scope
        processHandler(pkg, handlePackage(pkg.name, pkg,scope), newScope)
      case obj: Defn.Object =>
        val newScope = Scope.ObjScope(obj.symbol.toString()) :: scope
        processHandler(obj, handleObject(obj.symbol, obj,scope),newScope)
      case cls: Defn.Class =>
        val newScope = Scope.ClassScope(cls.symbol.displayName) :: scope
        processHandler(cls, handleClass(cls.symbol, cls,scope), newScope)
      case method: Defn.Def =>
        val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
        val fullSig = s"${method.symbol}:$typeSigs"
        val newScope = Scope.MethodScope(fullSig) :: scope
        processHandler(method, handleMethod(method.symbol, fullSig, method,scope), newScope)
      case valDef: Defn.Val =>
        val newScope = Scope.ValScope(valDef.symbol.displayName) :: scope
        processHandler(valDef, handleVal(valDef.symbol, valDef,scope),newScope)
      case varDef: Defn.Var =>
        val newScope = Scope.ValScope(varDef.symbol.displayName) :: scope
        processHandler(varDef, handleVar(varDef.symbol, varDef,scope),newScope)
      case _ =>
        println(s"Visiting ${tree.getClass} ${tree.symbol}")
        visitChildren(tree,scope)
    }
  }


  def handleVar(symbol: Symbol, varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean)
  def handleVal(symbol: Symbol, valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean)
  def handlePackage(packageName: Term.Name, pkg: Pkg, scope: List[Scope]): (Patch, Boolean)
  def handleMethod(objName: Symbol, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean)
  def handleObject(objName: Symbol, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean)
  def handleClass(clsSymbol: Symbol, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean)
}
