package scalaclean.util

import scalafix.patch.Patch
import scalafix.v1._

import scala.meta.{Decl, Defn, Import, Pkg, Term, Tree}

class DefaultTreeVisitor()(implicit doc: SemanticDocument) extends TreeVisitor {

  def handleVar(varDef: Defn.Var,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleVar $varDef - scope $scope")
    continue
  }

  def handleVar(varDef: Decl.Var,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleVar $varDef - scope $scope")
    continue
  }

  def handleVal(valDef: Defn.Val,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleVal $valDef - scope $scope")
    continue
  }
  def handleVal(valDef: Decl.Val,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleVal $valDef - scope $scope")
    continue
  }

  override def handlePackage(packageName: Term.Name, pkg: Pkg,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handlePackage $packageName - scope $scope")
    continue
  }

  override def handleMethod(methodName: Symbol, fullSig: String, method: Defn.Def,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleMethod $fullSig - scope $scope")
    continue
  }
  override def handleMethod(methodName: Symbol, fullSig: String, method: Decl.Def,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleMethod $fullSig - scope $scope")
    continue
  }

  override def handleObject(objName: Symbol, obj: Defn.Object,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleObject $objName - scope $scope")
    continue
  }

  override def handleClass(clsSymbol: Symbol, cls: Defn.Class,scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleClass $clsSymbol - scope $scope")
    continue
  }

  override def handleTrait(trtSymbol: Symbol, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean) = {
    println(s"handleTrait $trtSymbol - scope $scope")
    continue
  }

  override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = {
    continue
  }

  override def handleOther(tree: Tree, scope: List[Scope]): (Patch, Boolean) = {
    continue
  }

}
