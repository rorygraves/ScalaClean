package fix

import scalaclean.model.{ModelHelper, SCClass, SCModel}
import scalafix.v1._

import scala.collection.mutable.ListBuffer
import scala.meta.{Defn, Pkg, Source, Stat}

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanAnalysis
  */
class ScalaCleanDeadClass extends SemanticRule("ScalaCleanDeadClass")  {
  var model: SCModel = _


  override def beforeStart(): Unit = {
    println("Cleaner Rule BEFORE START")
    // TODO - where do we get the config path to load from - check other rules for examples

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
    println("Structure ----------------")
    model.printStructure()
    println("Structure end ----------------")

  }

  override def afterComplete(): Unit = {
    println("Cleaner Rule AFTER COMPLETE")
  }

  def isClassDead(name: String): Boolean = {
    // TODO This should depend on the SCModel
    println("ISDEAD = " + name)
    name == "fix/UnusedClass#"
  }

  def visitPkgStatements(pkg: String, statements: List[Stat])(implicit doc: SemanticDocument): Unit = {
    println(s"Package: $pkg")
    statements.foreach(visitPkgStatement(pkg,_))
  }

  def visitObject(pkg: String, obj: Defn.Object)(implicit doc: SemanticDocument): Unit = {
    println(s"Object = $pkg.${obj.name}  " + obj.mods.structureLabeled)

  }

  val toRemoveTokens = ListBuffer[meta.Token]()

  def visitClass(pkg: String, cls: Defn.Class, outerClass: Option[SCClass])(implicit doc: SemanticDocument): Unit = {
    val fullName = cls.name.symbol.toString()
    println(s"class = $fullName")
    if(isClassDead(fullName))
      toRemoveTokens ++= cls.tokens

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
      case p @ Pkg(pName, pstats) => // a package containing a sub-package
        println("PSYMBOL = " + p.symbol)
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
    Patch.removeTokens(toRemoveTokens.toList)
  }


}
