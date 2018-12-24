package scalaclean.privatiser

import scalaclean.model._
import scalaclean.util.{Scope, SymbolTreeVisitor, TokenHelper}
import scalafix.v1._

import scala.meta.Defn

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanDeadCodeAnalysis
  */
class ScalaCleanPrivatiserApply extends SemanticRule("ScalaCleanPrivatiserApply")  {
  object NotVisited extends Colour
  var model: ScalaCleanModel = _

  override def beforeStart(): Unit = {
    println("Cleaner Rule BEFORE START")
    // TODO - where do we get the config path to load from - check other rules for examples

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
  }

  val deadTargets = List(
    "scalaclean/deadcode/UnusedClass#",
    "scalaclean/deadcode/UsedClass#unusedMethod().:List(List(Int))"
  )

//  def isUnused(identifier: String): Boolean = {
//    // TODO This should depend on the inputFile
//    deadTargets.contains(identifier)
//  }

  override def fix(implicit doc: SemanticDocument): Patch = {

//    val tv = new SymbolTreeVisitor {
//
//      override protected def handlerSymbol(symbol: Symbol, defn: Defn, scope: List[Scope]): (Patch, Boolean) = {
//        if (isUnused(symbol.toString())) {
//          val tokens = defn.tokens
//          val firstToken = tokens.head
//
//          (Patch.removeTokens(TokenHelper.whitespaceTokensBefore(firstToken, doc.tokens)) + Patch.removeTokens(tokens), false)
//        } else
//          (Patch.empty, true)
//      }
//    }
//    tv.visitDocument(doc.tree)
    Patch.empty
  }
}
