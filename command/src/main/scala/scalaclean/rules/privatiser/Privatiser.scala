package scalaclean.rules.privatiser

import scalaclean.model._
import scalaclean.model.impl.ElementId
import scalaclean.rules.AbstractRule
import scalaclean.util.{Scope, SymbolTreeVisitor}
import scalafix.patch.Patch
import scalafix.v1.{SemanticDocument, SyntacticDocument}

import scala.collection.mutable.ListBuffer
import scala.meta.io.AbsolutePath
import scala.meta.tokens.Token
import scala.meta.tokens.Token.{KwDef, KwVal, KwVar}
import scala.meta.{Import, Mod, Pat, Stat}

class Privatiser(model: ProjectModel, debug: Boolean) extends AbstractRule("Privatiser", model, debug) {

  type Colour = PrivatiserLevel

  override def markInitial(): Unit = {
    model.allOf[ModelElement].foreach { e =>
      e.colour = Undefined
    }
  }

  override def runRule(): Unit = {
    model.allOf[ModelElement].foreach {
      case e: SourceModel =>
        e.colour = NoChange("source")
      case e => e.colour = localLevel(e)
    }
    model.allOf[ModelElement].toList.sortBy(_.infoPosSorted).foreach(ele =>
      println(s"$ele  colour: ${ele.colour}"))
  }

  def localLevel(element: ModelElement): PrivatiserLevel = {
    if (element.colour != Undefined) element.colour
    else {
      val incoming = {
        element.internalIncomingReferences map (_._1)
        }.toSet - element -- (element match {
        case v: ValModel => v.getter
        case v: VarModel => v.getter ++ v.setter
        case a: AccessorModel => a.field
        case _ => Nil
      })

      val enclosing = element.classOrEnclosing

      //is it defined by the signature
      var res: PrivatiserLevel = element match {
        case o: ObjectModel if o.xtends(ElementIds.AppObject) =>
          NoChange("its an App and needs to be public")
        //      case any: ModelElement if any.hasAnnotation[ExternalAccess] => Some(NoChange(any.getAnnotation[ExternalAccess]))
        //      case method:MethodModel if (method.overridesExternal)=>  res.combine(Level of parent method)
        //probably other cases
        case _ => Undefined
      }

      incoming foreach { ref: ModelElement =>
        val isFromChild = ref.classOrEnclosing.xtends(enclosing.modelElementId)
        val access = if (isFromChild)
          Scoped.Protected(ref.legacySymbol, s"accessed from $ref", forceProtected = false)
        else
          Scoped.Private(ref.legacySymbol, s"accessed from $ref").widen(Scoped.Private(element.legacySymbol, s"accessed from $ref"))
        res = res.widen(access)
      }
      //we must be visible to anything that overrides us
      element.internalDirectOverriddenBy foreach {
        overriddenBy =>
          res = res.widen(Scoped.Protected(overriddenBy.legacySymbol, s"overridden in from $overriddenBy", forceProtected = false))
      }

      //We must be at least as visible as anything that we override
      element.allDirectOverrides foreach {
        case (Some(overriddenModel), _) =>
          val overriddenVisibility = localLevel(overriddenModel) match {
            case s: Scoped if s.isProtected => s.copy(forceProtected = true)
            case other => other
          }
          res = res.widen(overriddenVisibility)
        case (None, _) =>
          //if it is not in the model, then we will leave this as is
          NoChange("inherits from external")


        //          val info = element.symbolInfo(parentSym)
        //          val reason = s"declared in $parentSym"
        //          val parentVis =
        //            if (info.isProtectedWithin) Scoped.Protected(info.within.get, reason, true)
        //            else if (info.isProtected) Scoped.Protected(v1.Symbol.RootPackage, reason, true)
        //            else if (info.isProtectedThis) ???
        //            else if (info.isPrivateWithin) Scoped.Private(info.within.get, reason)
        //            else if (info.isPrivate) ???
        //            else if (info.isPrivateThis) ???
        //            else Public(reason)
        //          res = res.widen(parentVis)
      }

      element.colour = res
      res
    }
  }
  var elementsObserved = 0
  var elementsChanged = 0

  override def printSummary(projectName: String): Unit =
    println (
      s"""Elements Observed = $elementsObserved
         |Elements Changed  = $elementsChanged
         |Effect rate       = ${(elementsChanged.toDouble / elementsObserved.toDouble * 10000).toInt / 100} %"
         |""".stripMargin)

  override def fix(targetFile: AbsolutePath, syntacticDocument: SyntacticDocument)(implicit semanticDocument: SemanticDocument): List[(Int, Int, String)] = {
    val lb = new ListBuffer[(Int, Int, String)]

    import scalafix.v1.{Patch => _, _}

    val tv: SymbolTreeVisitor = new SymbolTreeVisitor {

      override protected def handlerSymbol(
                                            symbol: ElementId, mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        if(symbol.isGlobal) {

          // TODO Check for not existing  model.getElement[ModelElement] match { case Some(ms) if(me.existsInsource)=> ... case None => continue }
          val modelElement = model.legacySymbol[ModelElement](symbol)
          if (modelElement.existsInSource) {
            val patch = changeAccessModifier(modelElement.colour, mods, stat, modelElement, None)
            //do we need to recurse into implementation?
            val rewriteContent = modelElement match {
              case _: ClassLike => true
              case _: MethodModel => false
              case _: FieldModel => throw new IllegalStateException(s"handlerPats should be called - $modelElement")
              case _: FieldsModel => throw new IllegalStateException(s"handlerPats should be called - $modelElement")
              case _: SourceModel => true
            }
            elementsObserved += 1
            if (patch != Patch.empty)
              elementsChanged += 1
            (patch, rewriteContent)
          } else continue
        } else
          continue
      }

      def info(sym: Symbol): Boolean = semanticDocument.info(sym).get.isProtectedWithin


      override protected def handlerPats(
                                          pats: Seq[Pat.Var], mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        //for vals and vars we set the access to the broadest of any access of the fields


        //        if(pats.head.symbol.isGlobal) {
        //          val modelElements: Seq[FieldOrAccessorModel] = pats.flatMap( p => model.getElement(FieldOrAccessorModel](ElementId(p.symbol))

        val modelElements: Seq[FieldOrAccessorModel] = pats map { p =>
          val ele = model.legacySymbol[FieldOrAccessorModel](ElementId(p.symbol))
          ele match {
            case v: ValModel => v
            case v: VarModel => v
            case a: AccessorModel => a.field.getOrElse(a)
            case _: ObjectModel => ???
          }
        }

        val combined = modelElements.foldLeft[PrivatiserLevel](Undefined) {
          case (level, v: VarModel) =>
            var res = level.widen(v.colour)
            v.getter.foreach { g =>
              if (g.colour ne null)
                res = res.widen(g.colour)
            }
            v.setter.foreach { s =>
              if (s.colour ne null)
                res = res.widen(s.colour)
            }
            res
          case (level, v: ValModel) =>
            var res = level.widen(v.colour)
            v.getter.foreach { g =>
              if (g.colour ne null)
                res = res.widen(g.colour)
            }
            res
          case (level, a: AccessorModel) =>
            level.widen(a.colour)
          case (_, _: ObjectModel) => ???

        }
        val keywordToken = modelElements.head match {
          case v: ValModel => stat.tokens.find(_.isInstanceOf[KwVal])
          case v: VarModel => stat.tokens.find(_.isInstanceOf[KwVar])
          case v: MethodModel => stat.tokens.find(_.isInstanceOf[KwDef])
          case _: ObjectModel => ???
        }
        assert(keywordToken.nonEmpty)
        if (modelElements.size == 1)
          assert(keywordToken.get.pos.start <= modelElements.head.rawEnd)
        else
          assert(keywordToken.get.pos.start <= modelElements.head.rawStart)
        val patch = changeAccessModifier(combined, mods, stat, modelElements.head, keywordToken)

        elementsObserved += 1
        if (patch != Patch.empty)
          elementsChanged += 1

        //we never need to recurse into RHS of decls as they are not externally visible
        (patch, false)
      }

      override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = continue

      def existingAccess(mods: Seq[Mod]): (Option[Mod], PrivatiserLevel) = {
        val res: Option[(Option[Mod], PrivatiserLevel)] = mods.collectFirst {
          case s@Mod.Private(scope) => (Some(s), Scoped.Private(ElementId.fromTree(scope), "existing"))
          case s@Mod.Protected(scope) => (Some(s), Scoped.Protected(ElementId.fromTree(scope), "existing", forceProtected = false))
        }
        res.getOrElse((None, Public("existing")))
      }

      private def changeAccessModifier(
                                        level: PrivatiserLevel, mods: Seq[Mod], defn: Stat, aModel: ModelElement, forcePosition: Option[Token]): Patch = {
        val (mod, existing) = existingAccess(mods)
        val proposed = level.asText(aModel)

        def buildInsertion(toReplace: String): Patch = {
          forcePosition match {
            case Some(token) =>
              lb.append((token.start, token.start, s"$toReplace "))
              Patch.addLeft(token, s"$toReplace ")
            case None =>
              val tokens = defn.tokens
              tokens.find {
                _.start == aModel.rawStart
              } match {
                case Some(token) =>
                  lb.append((token.start, token.start, s"$toReplace "))
                  Patch.addLeft(token, s"$toReplace ")
                case None =>
                  //probably quite worrying
                  lb.append((defn.pos.start, defn.pos.start, s"$toReplace "))
                  Patch.addLeft(defn, s"$toReplace ")
              }
          }
        }

        val structuredPatch =
        //          if (isWiderThanExisting(level, aModel, mod))
        //          Utils.addError(defn, s" cant widen to $proposed from $existing */")
        //        else
          (mod, level.shouldReplace(aModel), proposed) match {
            case (_, _, None) => Patch.empty
            case (None, _, Some(toReplace)) =>
              buildInsertion(toReplace)
            case (_, false, Some(toReplace)) =>
              buildInsertion(toReplace)
            case (Some(existing), true, Some(toReplace)) =>
              lb.append((existing.pos.start, existing.pos.end, s"$toReplace"))
              Patch.replaceTree(existing, s"$toReplace")
          }
        level.marker2(defn).foreach(v => lb.append(v))
        structuredPatch + level.marker(defn)

      }
    }

    tv.visitDocument(semanticDocument.tree)



    lb.toList.sortBy(_._1)
  }

}
