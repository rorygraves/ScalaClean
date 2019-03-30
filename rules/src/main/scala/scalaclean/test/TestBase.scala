package scalaclean.test

import scalaclean.model._
import scalaclean.util._
import scalafix.v1._

import scala.meta.{Decl, Defn, Import, Pat, Pkg, Stat, Term, Tree}

/**
  * A rule use to test the that incloming references ar set correctly,
  * needs to be run after ScalaCleanTestAnalysis
  */
abstract class TestBase(name: String) extends SemanticRule(name) {
  var model: ProjectModel = _

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

      def toPatch(str: String, stat: Stat): (Patch, Boolean) = {
        if ((str eq null) || str.isEmpty) {
          continue
        } else {
          (Patch.addRight(stat, s"/* $str */"), true)
        }
      }

      override def handleVar(varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean) = {
        handleVar(varDef, scope,varDef.pats )
      }
      override def handleVar(varDef: Decl.Var, scope: List[Scope]): (Patch, Boolean) = {
        handleVar(varDef, scope,varDef.pats )
      }
      def handleVar(varDef: Stat, scope: List[Scope], pats: List[Pat]): (Patch, Boolean) = {
        val patches = Utils.readVars(pats).map{
          varPattern =>
            val varModel = model.fromSymbol[VarModel](varPattern.symbol)
            visitVar( varModel)
        }
        toPatch(patches.filter(!_.isEmpty).mkString("*//*"), varDef)
      }

      override def handleVal(valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean) = {
        handleVal(valDef, scope, valDef.pats )
      }
      override def handleVal(valDef: Decl.Val, scope: List[Scope]): (Patch, Boolean) = {
        handleVal(valDef, scope,valDef.pats )
      }
      def handleVal(valDef: Stat, scope: List[Scope], pats: List[Pat]): (Patch, Boolean) = {
        val patches = Utils.readVars(pats).map{
          valPattern =>
            val valModel = model.fromSymbol[ValModel](valPattern.symbol)
            visitVal( valModel)
        }
        toPatch(patches.filter(!_.isEmpty).mkString("*//*"), valDef)
      }
      override def handlePackage(packageName: Term.Name, pkg: Pkg, scope: List[Scope]): (Patch, Boolean) = {
        continue
      }

      override def handleMethod(symbol: Symbol, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitMethod(model.fromSymbol[MethodModel](symbol)), method)
      }

      override def handleMethod(symbol: Symbol, fullSig: String, method: Decl.Def, scope: List[Scope]): (Patch, Boolean) = {
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


      override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = continue

      override def handleOther(tree: Tree, scope: List[Scope]): (Patch, Boolean) = continue
    }
    visiter.visitDocument(doc.tree)
  }

}
