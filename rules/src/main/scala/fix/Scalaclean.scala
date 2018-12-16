package fix

import scalafix.v1._

import scala.meta.{Defn, _}

class ScalaClean extends SemanticRule("ScalaClean") {


  def visitPkgStatements(pkg: String, statements: List[Stat]): Unit = {
    statements.foreach(visitPkgStatement(pkg,_))
  }

  def visitObject(pkg: String, obj: Defn.Object): Unit = {
    println(s"Object = $pkg.${obj.name}  " + obj.mods.structureLabeled)

  }

  def visitClass(pkg: String, cls: Defn.Class): Unit = {
    println(s"class = ${cls.name}  " + cls.mods.structureLabeled)
  }

  def visitPkgStatement(pkg: String, statement: Stat): Unit = {
    statement match {
      case Pkg(name, pstats) => // a package containing a sub-package
        val newPkg = s"$pkg.$name"
        println(s"Package: $newPkg")
        visitPkgStatements(newPkg, pstats)
      case o : Defn.Object =>
        visitObject(pkg, o)
      case c: Defn.Class =>
        visitClass(pkg, c)
      case _ =>
        throw new IllegalStateException()
      case Defn.Def(mods,name,_,_,_,_) =>
        println(s"  method = $name  " + mods.structureLabeled)
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    println("Tree.structureLabeled: " + doc.tree.structureLabeled)
    doc.tree match {
      case Source(stats) =>
        visitPkgStatements("_root_", stats)
      case _ =>
        throw new IllegalStateException(s"document: ${doc.input} does not start with a Source")
    }
    Patch.empty
  }
}
