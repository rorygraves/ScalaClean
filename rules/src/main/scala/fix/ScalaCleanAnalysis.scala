package fix

import scalaclean.model.{ModelHelper, SCClass, SCModel}
import scalafix.v1._

import scala.meta.{Defn, _}

class ScalaCleanAnalysis extends SemanticRule("ScalaCleanAnalysis")  {
  val model = new SCModel()

  override def beforeStart(): Unit = {
    println("Analysis BEFORE START")
  }

  override def afterComplete(): Unit = {
    ModelHelper.model = Some(model)
    println("Analysis AFTER COMPLETE")
  }

  def visitPkgStatements(pkg: String, statements: List[Stat])(implicit doc: SemanticDocument): Unit = {
    println(s"Package: $pkg")
    statements.foreach(visitPkgStatement(pkg,_))
  }

  def visitObject(pkg: String, obj: Defn.Object)(implicit doc: SemanticDocument): Unit = {
    println(s"Object = $pkg.${obj.name}  " + obj.mods.structureLabeled)

  }

  def visitClass(cls: Defn.Class, outerClass: Option[SCClass])(implicit doc: SemanticDocument): Unit = {
    val fullName = cls.name.symbol.toString()
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

  def visitPkgStatement(pkg: String, statement: Stat)(implicit doc: SemanticDocument): Unit = {
    statement match {
      case Pkg(pName, pstats) => // a package containing a sub-package
        
        val newPkg = s"$pkg.$pName"
        visitPkgStatements(newPkg, pstats)
      case o : Defn.Object =>
        visitObject(pkg, o)
      case c: Defn.Class =>
        visitClass(c, None)
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
