package scalaclean.deadcode

import scalaclean.util.TreeVisitor
import scalaclean.model._
import scalafix.v1._

import scala.meta.Defn

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanDeadCodeAnalysis
  */
class ScalaCleanDeadCodeRemover extends SemanticRule("ScalaCleanDeadCodeRemover")  {
  object NotVisited extends Colour
  var model: ScalaCleanModel = _


  def markInitial = {
    val initialColour = List(NotVisited)
    model.allOf[ModelElement].foreach {
      e => e.colours = initialColour
    }
  }
  def isMainMethod(method:MethodModel) = {
    method.name == "main" //TODO

  }
  def allEntryPoints = {
    for (obj <- model.allOf[ObjectModel];
         method <- obj.methods
         if isMainMethod(method)) yield method
  }

  def markUsed(element:ModelElement): Unit = {
    if (element.colours.nonEmpty) {
      element.colours == Nil
      element match {
        case method: MethodModel =>
        //todo recurse based on the type of what is found
        //  method.body ???

      }
      //todo recurse based on the enclosing element
      //markUsed(element.owner)
    }
  }
  override def beforeStart(): Unit = {
    println("Cleaner Rule BEFORE START")
    // TODO - where do we get the config path to load from - check other rules for examples

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))


    markInitial

    val entries = allEntryPoints
    entries foreach markUsed
    println("Structure ----------------")
    model.printStructure()
    println("Structure end ----------------")
  }

  val deadTargets = List(
    "scalaclean/deadcode/UnusedClass#",
    "scalaclean/deadcode/UsedClass#unusedMethod().:List(List(Int))"
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
        if (isUnused(fullSig))
          (Patch.removeTokens(method.tokens), false)
        else
          (Patch.empty, true)
      }
    }

    tv.visitDocument(doc.tree)
  }
}
