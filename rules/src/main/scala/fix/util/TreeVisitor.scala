package fix.util

import scalafix.patch.Patch
import scalafix.v1._

import scala.meta.{Defn, Pkg, Source, Stat, Term, Tree}

class TreeVisitor()(implicit doc: SemanticDocument) {

  def visitDocument(tree: Tree): Patch = {
    tree match {
      case Source(stats) =>
        stats.foldLeft(Patch.empty) { case (patch, stmt) =>
          patch + visitStatement(stmt)
        }
      case _ =>
        throw new IllegalStateException(s"document: ${doc.input} does not start with a Source")
    }
  }

  def visitPackage(pkg: Pkg): Patch = {
    val (patch, traverseChildren) = handlePackage(pkg.name, pkg)
    if(traverseChildren) {
      println("PKG STMTS SIZE = " + pkg.stats.length)
      pkg.stats.foldLeft(patch) { case (patch, stmt) =>
//        println("PKG STMT: " + stmt)
        println("PKG STMT: " + stmt)
        patch + visitStatement(stmt)
//        Patch.empty
      case _ =>
        println("PKG STMT: OTHER")
          Patch.empty
      }
    } else
      patch
  }

  def handlePackage(packageName: Term.Name, pkg: Pkg): (Patch, Boolean) = {
    (Patch.empty, true)
  }

  def visitStatements(stmts: List[Stat]): Patch = {
    stmts.foldLeft(Patch.empty) { case (patch, stmt) =>
        patch + visitStatement(stmt)
    }
  }

  def visitStatement(statement: Stat): Patch = {
    statement match {
      case pkg : Pkg =>
        visitPackage(pkg)
      case o : Defn.Object =>
        visitObject(o)
      case c: Defn.Class =>
        visitClass(c)
      case m: Defn.Def =>
        visitMethod(m)
      case s =>
        throw new IllegalStateException("ERROR unknown statement: " + s)
    }
  }

  def visitBodyStatement(statement: Stat): Patch = {
    statement match {
      case u =>
        println("Warning - Unknown statement in body - " + u.getClass)
        Patch.empty
    }
  }


  def visitMethod(method: Defn.Def): Patch = {
    val typeSigs = method.paramss.map(_.map(v=> v.decltpe.get)).toString
    val fullSig = s"${method.symbol}:$typeSigs"
    println(s"Method = $fullSig" )
    println()
    val (patch, traverseChildren) = handleMethod(method.symbol,fullSig, method)
    if(traverseChildren) {
      patch + visitBodyStatement(method.body)
    } else
      patch
  }

  def handleMethod(objName: Symbol, fullSig: String, method: Defn.Def): (Patch, Boolean) = {
    (Patch.empty, true)
  }

  def visitObject(obj: Defn.Object): Patch = {
    println(s"Object = ${obj.symbol}")
    val (patch, traverseChildren) = handleObject(obj.symbol, obj)
    if(traverseChildren) {
      visitStatements(obj.templ.stats)
    } else
      patch
  }

  def handleObject(objName: Symbol, obj: Defn.Object): (Patch, Boolean) = {
    (Patch.empty, true)
  }

  private def visitClass(cls: Defn.Class): Patch = {
    val (patch, traverseChildren) = handleClass(cls.symbol, cls)
    if(traverseChildren) {
      // Parent def?
      println("CLASS PARENT? = " + cls.parent.map(_.symbol))
      visitStatements(cls.templ.stats)
    } else
      patch
  }

  def handleClass(clsSymbol: Symbol, cls: Defn.Class): (Patch, Boolean) = {
    (Patch.empty, true)
  }
}
