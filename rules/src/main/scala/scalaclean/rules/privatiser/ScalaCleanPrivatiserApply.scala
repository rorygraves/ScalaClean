//package scalaclean.rules.privatiser
//
//import scala.meta.tokens.{Token, Tokens}
//import scalaclean.model._
//import scalaclean.util._
//import scalafix.v1._
//
//import scala.meta.{Decl, Defn, Mod}
//
///**
//  * A rule that removes unreferenced classes,
//  * needs to be run after Analysis
//  */
//class ScalaCleanPrivatiserApply extends SemanticRule("Privatiser") with SymbolUtils {
//  var model: ScalaCleanModel = _
//
//  override def beforeStart(): Unit = {
//    println("Cleaner Rule Privatiser BEFORE START")
//    // TODO - where do we get the config path to load from - check other rules for examples
//
//    // hack to load the model from the helper class
//    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
//    println(s"Cleaner Rule Privatiser size ${model.allOf[ModelElement].size}")
//    model.allOf[ModelElement].foreach { e =>
//      e.colour = calcLevel(e)
//    }
//    println("Cleaner Rule Privatiser BEFORE START END")
//  }
//
//  private def calcLevel(element: ModelElement): PrivatiserLevel = {
//    val incoming = {
//      element.internalIncomingReferences map (_._1)
//    }.toSet - element
//    println(s"Privatiser for $element START - process ${element.internalIncomingReferences}")
//
//    var res: PrivatiserLevel = Public(element.symbol)
//    //is it defined by the signature
//    element match {
//      case o: ObjectModel if o.xtends[App] =>
//        res.combine(NoChange(element.symbol, "its an App and needs to be public"))
//      //      case any: ModelElement if any.hasAnnotation[ExternalAccess] => Some(NoChange(any.getAnnotation[ExternalAccess]))
//      //      case method:MethodModel if (method.overidesExternal)=>  res.combine(Level of parent method)
//      //probably other cases
//      case _ =>
//    }
//
//    incoming foreach { ref =>
//      val access = Private(findCommonParent(ref.symbol, element.symbol), s"accessed from $ref")
//      res = res.combine(access)
//    }
//    //if it is the same as currently assigned
//    // res = NoChange(s" no change - calc was $res")
//    //if no access
//    println(s"Privatiser for $element END - $res")
//    res
//  }
//
//  override def fix(implicit doc: SemanticDocument): Patch = {
//    import scalafix.v1._
//
//    val tv = new DefaultTreeVisitor {
//
//      // TODO - comment these methods once the model is complete
//
//      //      override def handleVar(symbol: Symbol, varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean) =
//      //        handleDefn(symbol, varDef, scope, traverseChildren = false)
//
//      //      override def handleVal(symbol: Symbol, valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean) =
//      //        handleDefn(symbol, valDef, scope, traverseChildren = false)
//
//      //      override def handleMethod(methodName: Symbol, fullSig: String, method: Defn.Def, scope: List[Scope]): (Patch, Boolean) =
//      //        handleDefn(methodName, method, scope, traverseChildren = false)
//
//      //      override def handleVar(symbol: Symbol, varDef: Decl.Var, scope: List[Scope]): (Patch, Boolean) =
//      //        handleDefn(symbol, varDef, scope, traverseChildren = false)
//
//      //      override def handleVal(symbol: Symbol, valDef: Decl.Val, scope: List[Scope]): (Patch, Boolean) =
//      //        handleDefn(symbol, valDef, scope, traverseChildren = false)
//
//      //      override def handleMethod(methodName: Symbol, fullSig: String, method: Decl.Def, scope: List[Scope]): (Patch, Boolean) =
//      //        handleDefn(methodName, method, scope, traverseChildren = false)
//
////      override def handleClass(clsSymbol: Symbol, cls: Defn.Class, scope: List[Scope]): (Patch, Boolean) =
////        handleDefn(clsSymbol, cls, scope, traverseChildren = true)
//
//      override def handleVar(varDef: Defn.Var, scope: List[Scope]): (Patch, Boolean) = ???
//
//      override def handleVar(varDef: Decl.Var, scope: List[Scope]): (Patch, Boolean) = ???
//
//      override def handleVal(valDef: Defn.Val, scope: List[Scope]): (Patch, Boolean) = ???
//
//      override def handleVal(valDef: Decl.Val, scope: List[Scope]): (Patch, Boolean) = ???
//
//      override def handleTrait(trtSymbol: Symbol, cls: Defn.Trait, scope: List[Scope]): (Patch, Boolean) =
//        handleDefn(trtSymbol, cls, scope, traverseChildren = true)
//
//      override def handleObject(objName: Symbol, obj: Defn.Object, scope: List[Scope]): (Patch, Boolean) =
//        handleDefn(objName, obj, scope, traverseChildren = true)
//
//      private def handleDefn[T <: Defn](objName: Symbol, defn: T, scope: List[Scope], traverseChildren: Boolean): (Patch, Boolean) = {
//        val prepared = model.fromSymbol[ObjectModel](defn.symbol)
//        val change = prepared.colour.asInstanceOf[PrivatiserLevel]
//        change match {
//          case NoChange(_, _) => continue
//          case Public(_) =>
//            // TODO - uncomment once all stubs are complete, until then Public can happen
//            //throw new IllegalStateException("Trying to make something public, something went terribly wrong here!")
//            continue
//          case level: PrivatiserLevel =>
//            changeAccessModifier(level, defn, traverseChildren)
//        }
//      }
//    }
//    tv.visitDocument(doc.tree)
//  }
//
//  private def changeAccessModifier[T <: Defn](level: PrivatiserLevel, defn: T, traverseChildren: Boolean): (Patch, Boolean) =
//    level.keyword.fold {
//      (Patch.empty, true)
//    } { kwd =>
//      val accessModifiersToRemove = defn.tokens.filter(t => t.isInstanceOf[Token.KwPrivate] || t.isInstanceOf[Token.KwProtected])
//      val patch = Patch.addLeft(defn, s"$kwd ") + Patch.removeTokens(accessModifiersToRemove)
//      (patch, traverseChildren)
//    }
//}
