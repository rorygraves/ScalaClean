package fix

import fix.util.TreeVisitor
import scalaclean.model.{ModelHelper, ScalaCleanModel}
import scalafix.v1._

import scala.meta.Defn

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanAnalysis
  */
class ScalaCleanDeadClass extends SemanticRule("ScalaCleanDeadClass") {
  var model: ScalaCleanModel = _


  override def beforeStart(): Unit = {
    println("Cleaner Rule BEFORE START")
    // TODO - where do we get the config path to load from - check other rules for examples

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
  }

  val deadTargets = List(
    "fix/UnusedClass#",
//    "fix/UsedClass#unusedMethod().:List(List(Int))"
  )


  def isUnused(identifier: String): Boolean = {
    // TODO This should depend on the inputFile
    deadTargets.contains(identifier)
  }

  override def fix(implicit doc: SemanticDocument): Patch = {

    val tv = new TreeVisitor {
      override def handleClass(clsSymbol: Symbol, cls: Defn.Class): (Patch, Boolean) = {
        if (isUnused(clsSymbol.toString()))
          (Patch.removeTokens(cls.tokens), false)
        else
          (Patch.empty, true)
      }

      override def handleMethod(objName: Symbol, fullSig: String, method: Defn.Def): (Patch, Boolean) = {
        println("AAAA - here = " + fullSig)
        if (isUnused(fullSig))
          (Patch.removeTokens(method.tokens), false)
        else
          (Patch.empty, true)
      }
    }

    tv.visitDocument(doc.tree)
  }
}
