package scalaclean.privatiser

import scalaclean.model._
import scalaclean.util._
import scalafix.v1._

import scala.meta.Defn

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanPrivatiserAnalysis
  */
class ScalaCleanPrivatiserApply extends SemanticRule("ScalaCleanPrivatiserApply") with AnalyserUtils {
  var model: ScalaCleanModel = _

  override def beforeStart(): Unit = {
    println("Cleaner Rule Privatiser BEFORE START")
    // TODO - where do we get the config path to load from - check other rules for examples

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
    println(s"Cleaner Rule Privatiser size ${model.allOf[ModelElement].size}")
    model.allOf[ModelElement].foreach { e =>
      e.colour = calcLevel(e)
    }
    println("Cleaner Rule Privatiser BEFORE START END")
  }

  private def calcLevel(element: ModelElement): PrivatiserLevel = {
    val incoming = {
      element.internalIncomingReferences map (_._1)
    }.toSet - element
    println(s"Privatiser for $element START - process ${element.internalIncomingReferences}")

    var res: PrivatiserLevel = Public(element.symbol)
    //is it defined by the signature
    element match {
      case o: ObjectModel if o.xtends[App] =>
        res.combine(NoChange(element.symbol, "its an App and needs to be public"))
      //      case any: ModelElement if any.hasAnnotation[ExternalAccess] => Some(NoChange(any.getAnnotation[ExternalAccess]))
      //      case method:MethodModel if (method.overidesExternal)=>  res.combine(Level of parent method)
      //probably other cases
      case _ =>
    }

    incoming foreach { ref =>
      val access = Private(findCommonParent(ref.symbol, element.symbol), s"accessed from $ref")
      res = res.combine(access)
    }
    //if it is the same as currently assigned
    // res = NoChange(s" no change - calc was $res")
    //if no access
    println(s"Privatiser for $element END - $res")
    res
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    import scalafix.v1._

    val tv = new DefaultTreeVisitor {

//      override def handleVar(symbol: Symbol, varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean) = super.handleVar(symbol, varDef, scope)
//      override def handleVal(symbol: Symbol, valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean) = super.handleVal(symbol, valDef, scope)
//      override def handleMethod(methodName: Symbol, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean) = super.handleMethod(methodName, fullSig, method, scope)
//      override def handleClass(clsSymbol: Symbol, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean) = super.handleClass(clsSymbol, cls, scope)
//      override def handleTrait(trtSymbol: Symbol, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean) = super.handleTrait(trtSymbol, cls, scope)

      override def handleObject(objName: Symbol, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean) = {
        val prepared = model.fromSymbol[ObjectModel](obj.symbol)
        val change = prepared.colour.asInstanceOf[PrivatiserLevel]
        change match {
          case NoChange(_, _) => (Patch.empty, true)
          case Public(_) => throw new IllegalStateException("Trying to make something public, something went terribly wrong here!")
          case level: PrivatiserLevel => changeAccessModifier(level)
        }
      }
    }
    tv.visitDocument(doc.tree)
    Patch.empty
  }

  private def changeAccessModifier(level: PrivatiserLevel): (Patch, Boolean) = {
    val symbol = level

    // TODO -- change me
    (Patch.empty, true)
  }
}
