package fix

import scalaclean.model.{SCClass, SCModel}
import scalafix.v1._

import scala.meta.{Defn, _}

class ScalaCleanAnalysis extends SemanticRule("ScalaCleanAnalysis")  {
  val model = new SCModel()

  override def beforeStart(): Unit = {
    println("BEFORE START")
  }

  override def afterComplete(): Unit = {
    println("AFTER COMPLETE")
    model.printStructure()
  }

  def visitPkgStatements(pkg: String, statements: List[Stat]): Unit = {
    println(s"Package: $pkg")
    statements.foreach(visitPkgStatement(pkg,_))
  }

  def visitObject(pkg: String, obj: Defn.Object): Unit = {
    println(s"Object = $pkg.${obj.name}  " + obj.mods.structureLabeled)

  }

  def visitClass(pkg: String, cls: Defn.Class, outerClass: Option[SCClass]): Unit = {
    val fullName = s"$pkg.${cls.name}"
    val scCls = model.getOrCreateClass(fullName)
    scCls.setOuter(outerClass)
    println(s"class = $fullName")
    cls.templ.stats.foreach {
      case vl : Defn.Val =>
      case vr : Defn.Var =>
      case df @ Defn.Def(mods,defName,_,_,_,_) =>
        println(s"  method = $defName  " + mods.structureLabeled)

      case dc: Defn.Class => // Inner class

    }
  }

  def visitPkgStatement(pkg: String, statement: Stat): Unit = {
    statement match {
      case Pkg(pName, pstats) => // a package containing a sub-package
        val newPkg = s"$pkg.$pName"
        visitPkgStatements(newPkg, pstats)
      case o : Defn.Object =>
        visitObject(pkg, o)
      case c: Defn.Class =>
        visitClass(pkg, c, None)
      case _ =>
        throw new IllegalStateException()
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
//    println("Tree.structureLabeled: " + doc.tree.structureLabeled)
    doc.tree match {
      case Source(stats) =>
        visitPkgStatements("_root_", stats)
      case _ =>
        throw new IllegalStateException(s"document: ${doc.input} does not start with a Source")
    }
    Patch.empty
  }

}
