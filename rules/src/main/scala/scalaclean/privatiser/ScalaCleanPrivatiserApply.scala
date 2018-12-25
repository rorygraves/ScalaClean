package scalaclean.privatiser

import scalaclean.model._
import scalaclean.util.{Scope, SymbolTreeVisitor, TokenHelper}
import scalafix.v1._

import scala.meta.Defn

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanPrivatiserAnalysis
  */
class ScalaCleanPrivatiserApply extends SemanticRule("ScalaCleanPrivatiserApply")  {
  var model: ScalaCleanModel = _

  sealed trait PrivatiserLevel extends Colour
  case class Protected(name:String) extends PrivatiserLevel
  case class Private(name:String) extends PrivatiserLevel
  case object NoChange extends PrivatiserLevel

  def calcLevel(element: ModelElement): PrivatiserLevel = {
    val incoming = {element.internalIncomingReferences map (_._1)}.toSet - element
    println(s"Privatiser for $element START - process ${element.internalIncomingReferences}")

    incoming foreach {
      ref =>
      println (s"$element is referred to by $ref")
    }

    println(s"Privatiser for $element END")
    NoChange
  }

  override def beforeStart(): Unit = {
    println("Cleaner Rule Privatiser BEFORE START")
    // TODO - where do we get the config path to load from - check other rules for examples

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
    println(s"Cleaner Rule Privatiser size ${model.allOf[ModelElement].size}")
    model.allOf[ModelElement].foreach {
      e => e.colour = calcLevel(e)
    }
    println("Cleaner Rule Privatiser BEFORE START END")
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
