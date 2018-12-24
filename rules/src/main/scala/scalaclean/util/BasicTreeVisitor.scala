package scalaclean.util

import scalafix.patch.Patch
import scalafix.v1._

import scala.meta.internal.semanticdb.Scala.Names.TermName
import scala.meta.{Defn, Pkg, Term, Tree}

abstract class BasicTreeVisitor()(implicit doc: SemanticDocument) {

  final def visitDocument(tree: Tree): Unit = {
    visitTree(tree)
  }
  type Child
  protected def beforeChildren(): Child
  protected def afterChildren(value: Child): Unit
  private def visitChildren(t: Tree): Unit = {
    val before = beforeChildren()
    t.children.foreach {visitTree}
    afterChildren(before)
  }

  private def processHandler(tree: Tree, handleRes: Boolean): Unit =
    if (handleRes)
      visitChildren(tree)


  def visitTree(tree: Tree): Unit = {

    tree match {
      case pkg: Pkg =>
        processHandler(pkg, handlePackage(pkg.name, pkg))
      case obj: Defn.Object =>
        processHandler(obj, handleObject(obj.symbol, obj))
      case cls: Defn.Class =>
        processHandler(cls, handleClass(cls.symbol, cls))
      case cls: Defn.Trait =>
        processHandler(cls, handleTrait(cls.symbol, cls))
      case method: Defn.Def =>
        val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
        val fullSig = s"${method.symbol}:$typeSigs"
        processHandler(method, handleMethod(method.symbol, fullSig, method))
      case valDef: Defn.Val =>
        processHandler(valDef, handleVal(valDef.symbol, valDef))
      case varDef: Defn.Var =>
        processHandler(varDef, handleVar(varDef.symbol, varDef))
      case other: Tree=>
        processHandler(other, handleOther(other.symbol, other))
//      case x =>
//        println(s"Visiting unknown ${tree.getClass} ${tree.symbol}")
//        visitChildren(tree)
    }
  }


  def handleVar(symbol: Symbol, varDef: Defn.Var):Boolean
  def handleVal(symbol: Symbol, valDef: Defn.Val):Boolean
  def handlePackage(packageName: Term.Name, pkg: Pkg):Boolean
  def handleMethod(objName: Symbol, fullSig: String, method: Defn.Def):Boolean
  def handleObject(objName: Symbol, obj: Defn.Object):Boolean
  def handleClass(clsSymbol: Symbol, cls: Defn.Class):Boolean
  def handleTrait(trtSymbol: Symbol, cls: Defn.Trait):Boolean
  def handleOther(otSymbol: Symbol, other: Tree):Boolean
}
