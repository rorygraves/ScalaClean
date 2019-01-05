package scalaclean.test

import scalaclean.model._
import scalaclean.util._
import scalafix.v1._

import scala.meta.{Defn, Pkg, Term}

/**
  * A rule use to test the that incloming references ar set correctly,
  * needs to be run after ScalaCleanTestAnalysis
  */
abstract class TestBase(name: String) extends SemanticRule(name) with SymbolUtils {
  var model: ScalaCleanModel = _

  override def beforeStart(): Unit = {
    println(s"Test Rule $name beforeStart START")

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
    println(s"Test Rule $name size ${model.allOf[ModelElement].size}")

    println(s"Cleaner Rule $name beforeStart END")
  }

  def visitVar(element: VarModel): String
  def visitVal(element: ValModel): String
  def visitMethod(element: MethodModel):String
  def visitObject(element: ObjectModel):String
  def visitClass(element: ClassModel): String
  def visitTrait(element: TraitModel): String

  override def fix(implicit doc: SemanticDocument): Patch = {
    import scalafix.v1._

    object visiter extends TreeVisitor {

      def toPatch(str: String, defn: Defn): (Patch, Boolean) = {
        if ((str eq null) || str.isEmpty) {
          (Patch.empty, true)
        } else {
          (Patch.addRight(defn, s"/* $str */"), true)
        }
      }

      override def handleVar(symbol: Symbol, varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitVar( model.fromSymbol[VarModel](symbol)), varDef)
      }

      override def handleVal(symbol: Symbol, valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitVal(model.fromSymbol[ValModel](symbol)), valDef)
      }

      override def handlePackage(packageName: Term.Name, pkg: Pkg, scope: List[Scope]): (Patch, Boolean) = {
        (Patch.empty, true)
      }

      override def handleMethod(symbol: Symbol, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitMethod(model.fromSymbol[MethodModel](symbol)), method)
      }

      override def handleObject(symbol: Symbol, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitObject(model.fromSymbol[ObjectModel](symbol)), obj)
      }

      override def handleClass(symbol: Symbol, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitClass(model.fromSymbol[ClassModel](symbol)), cls)
      }

      override def handleTrait(symbol: Symbol, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitTrait(model.fromSymbol[TraitModel](symbol)), cls)
      }
    }
    visiter.visitDocument(doc.tree)
  }

}
