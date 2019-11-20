package scalaclean.test

import scalaclean.model._
import scalaclean.model.impl.ElementId
import scalaclean.util._
import scalafix.v1._

import scala.meta.{Decl, Defn, Import, Pat, Pkg, Stat, Term, Tree}

/**
  * A rule use to test the that incoming references ar set correctly,
  * needs to be run after ScalaCleanTestAnalysis
  */
abstract class TestBase(name: String, model: ProjectModel) extends SemanticRule(name) {

  override def beforeStart(): Unit = {
    println(s"Test Rule $name beforeStart START")
    println(s"Test Rule $name size ${model.allOf[ModelElement].size}")
    println(s"Cleaner Rule $name beforeStart END")
  }

  def visitVar(element: VarModel): String

  def visitVal(element: ValModel): String

  def visitMethod(element: MethodModel): String

  def visitObject(element: ObjectModel): String

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
        handleVar(varDef, scope, varDef.pats)
      }

      override def handleVar(varDef: Decl.Var, scope: List[Scope]): (Patch, Boolean) = {
        handleVar(varDef, scope, varDef.pats)
      }

      def handleVar(varDef: Stat, scope: List[Scope], pats: List[Pat]): (Patch, Boolean) = {
        val patches = Utils.readVars(pats).map {
          varPattern =>
            val varModel = model.legacySymbol[VarModel](ElementId(varPattern.symbol))
            visitVar(varModel)
        }
        toPatch(patches.filter(!_.isEmpty).mkString("*//*"), varDef)
      }

      override def handleVal(valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean) = {
        handleVal(valDef, scope, valDef.pats)
      }

      override def handleVal(valDef: Decl.Val, scope: List[Scope]): (Patch, Boolean) = {
        handleVal(valDef, scope, valDef.pats)
      }

      def handleVal(valDef: Stat, scope: List[Scope], pats: List[Pat]): (Patch, Boolean) = {
        val patches = Utils.readVars(pats).map {
          valPattern =>
            val valModel = model.legacySymbol[ValModel](ElementId(valPattern.symbol))
            visitVal(valModel)
        }
        toPatch(patches.filter(!_.isEmpty).mkString("*//*"), valDef)
      }

      override def handlePackage(packageName: Term.Name, pkg: Pkg, scope: List[Scope]): (Patch, Boolean) = {
        continue
      }

      override def handleMethod(symbol: ElementId, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitMethod(model.legacySymbol[MethodModel](symbol)), method)
      }

      override def handleMethod(symbol: ElementId, fullSig: String, method: Decl.Def, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitMethod(model.legacySymbol[MethodModel](symbol)), method)
      }

      override def handleObject(symbol: ElementId, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitObject(model.legacySymbol[ObjectModel](symbol)), obj)
      }

      override def handleClass(symbol: ElementId, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitClass(model.legacySymbol[ClassModel](symbol)), cls)
      }

      override def handleTrait(symbol: ElementId, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean) = {
        toPatch(visitTrait(model.legacySymbol[TraitModel](symbol)), cls)
      }


      override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = continue

      override def handleOther(tree: Tree, scope: List[Scope]): (Patch, Boolean) = continue
    }
    visiter.visitDocument(doc.tree)
  }

}
