package scalaclean.privatiser

import scalaclean.model._
import scalaclean.util._
import scalafix.v1._

import scala.meta.Defn


/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanPrivatiserAnalysis
  */
class ScalaCleanPrivatiserApply extends SemanticRule("ScalaCleanPrivatiserApply")  {
  var model: ScalaCleanModel = _

  sealed trait PrivatiserLevel extends Colour {
    def reason: String
    def combine(level: PrivatiserLevelAdd): PrivatiserLevel
  }
  sealed trait PrivatiserLevelAdd extends PrivatiserLevel
  //  case class Protected(scope:Symbol, reason: String) extends PrivatiserLevelAdd
  case class Private(scope:Symbol, reason: String) extends PrivatiserLevelAdd {
    override def combine(level: PrivatiserLevelAdd): PrivatiserLevel = {
      level match {
        case NoChange(_) => level
        case Private(otherScope, otherReason) =>
      val commonParent = findCommonParent(scope, otherScope)
      if (commonParent == scope) this
      else if (commonParent == otherScope) level
      else Private(commonParent, s"common parent of '$reason' and '$otherReason'")
      }
    }
  }

  protected def findCommonParent(scope1:Symbol, scope2:Symbol): Symbol = {
    def depth(scope: Symbol): Int = {
      if (scope.isNone) 0 else depth(scope.owner)
    }

    val depth2 = depth(scope2)

    def parent(scope: Symbol, level: Int): Symbol = {
      if (level == 0) scope else parent(scope.owner, level - 1)
    }

    val depth1 = depth(scope1)
    if (depth1 > depth2) {
      findCommonParent(parent(scope1, depth1 - depth2), scope2)
    } else if (depth2 > depth1) {
      findCommonParent(scope1, parent(scope2, depth2 - depth1))
    } else if (scope1 == scope2) scope1
    else findCommonParent(scope1.owner, scope2.owner)
  }

  case class NoChange(reason: String) extends PrivatiserLevelAdd {
    override def combine(level: PrivatiserLevelAdd): PrivatiserLevel = this
  }
  case object Initial extends PrivatiserLevel {
    def reason = "initial"
    override def combine(level: PrivatiserLevelAdd): PrivatiserLevel = level
  }

  def calcLevel(element: ModelElement): PrivatiserLevel = {
    val incoming = {
      element.internalIncomingReferences map (_._1)
    }.toSet - element
    println(s"Privatiser for $element START - process ${element.internalIncomingReferences}")

    var res: PrivatiserLevel = Initial
    //is it defined by the signature
    element match {
      case o: ObjectModel if o.xtends[App] =>
        res.combine(NoChange("its an App and needs to be public"))
      //      case any: ModelElement if any.hasAnnotation[ExternalAccess] => Some(NoChange(any.getAnnotation[ExternalAccess]))
      //      case method:MethodModel if (method.overidesExternal)=>  res.combine(Level of parent method)
      //probably other cases
      case _ =>
    }


    incoming foreach {
      ref =>
        val access = Private(findCommonParent(ref.symbol, element.symbol), s"accessed from $ref")
        res = res.combine(access)
    }
    //if it is the same as currently assigned
    // res = NoChange(s" no change - calc was $res")
    //if no access
    println(s"Privatiser for $element END - $res")
    res
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
          case NoChange(reason) =>  (Patch.empty, true)
          case Private(scope, reason) =>
            //TODO
            (Patch.empty, true)
          case Initial =>  ???
        }
      }
    }
    tv.visitDocument(doc.tree)
    Patch.empty
  }
}
