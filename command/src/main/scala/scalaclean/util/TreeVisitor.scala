package scalaclean.util

import scalaclean.model.impl.ElementId
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
  val continue = (Patch.empty, true)
}
abstract class TreeVisitor()(implicit doc: SemanticDocument) {

  protected final def continue = TreeVisitor.continue
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


  def visitTree(tree: Tree, scope: List[Scope]): Patch = {

    tree match {
      case pkg: Pkg =>
        val newScope = Scope.PkgScope(pkg.name.toString()) :: scope
        processHandler(pkg, handlePackage(pkg.name, pkg, scope), newScope)
      case obj: Defn.Object =>
        val newScope = Scope.ObjScope(obj.symbol.toString()) :: scope
        processHandler(obj, handleObject(ElementId(obj.symbol), obj, scope), newScope)
      case cls: Defn.Class =>
        val newScope = Scope.ClassScope(cls.symbol.displayName) :: scope
        processHandler(cls, handleClass(ElementId(cls.symbol), cls, scope), newScope)
      case cls: Defn.Trait =>
        val newScope = Scope.TraitScope(cls.symbol.displayName) :: scope
        processHandler(cls, handleTrait(ElementId(cls.symbol), cls, scope), newScope)
      case method: Defn.Def =>
        val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
        val fullSig = s"${method.symbol}:$typeSigs"
        val newScope = Scope.MethodScope(fullSig) :: scope
        processHandler(method, handleMethod(ElementId(method.symbol), fullSig, method, scope), newScope)
      case method: Decl.Def =>
        val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
        val fullSig = s"${method.symbol}:$typeSigs"
        val newScope = Scope.MethodScope(fullSig) :: scope
        processHandler(method, handleMethod(ElementId(method.symbol), fullSig, method, scope), newScope)
      case valDef: Defn.Val =>
        val newScope = Scope.ValScope(valDef.symbol.displayName) :: scope
        processHandler(valDef, handleVal(valDef, scope), newScope)
      case valDef: Decl.Val =>
        val newScope = Scope.ValScope(valDef.symbol.displayName) :: scope
        processHandler(valDef, handleVal(valDef, scope), newScope)
      case varDef: Defn.Var =>
        val newScope = Scope.ValScope(varDef.symbol.displayName) :: scope
        processHandler(varDef, handleVar(varDef, scope), newScope)
      case varDef: Decl.Var =>
        val newScope = Scope.ValScope(varDef.symbol.displayName) :: scope
        processHandler(varDef, handleVar(varDef, scope), newScope)
      case importStat : Import =>
        processHandler(importStat, handleImport(importStat, scope), scope)
      case _ =>
//        println(s"Visiting ${tree.getClass} ${tree.symbol}")
        processHandler(tree, handleOther(tree, scope), scope)
    }
  }


  def handleVar(varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean)
  def handleVar(varDef: Decl.Var, scope: List[Scope]): (Patch, Boolean)

  def handleVal(valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean)
  def handleVal(valDef: Decl.Val, scope: List[Scope]): (Patch, Boolean)

  def handleMethod(symbol: ElementId, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean)
  def handleMethod(symbol: ElementId, fullSig: String, method: Decl.Def, scope: List[Scope]): (Patch, Boolean)

  def handleObject(symbol: ElementId, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean)

  def handleClass(symbol: ElementId, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean)

  def handleTrait(symbol: ElementId, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean)

  //non model entries
  def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean)
  def handlePackage(packageName: Term.Name, pkg: Pkg, scope: List[Scope]): (Patch, Boolean)
  def handleOther(tree: Tree, scope: List[Scope]): (Patch, Boolean)
}
