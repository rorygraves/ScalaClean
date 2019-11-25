package scalaclean.rules.deadcode

import scalaclean.model._
import scalaclean.model.impl.ElementId
import scalaclean.rules.AbstractRule
import scalaclean.util.{Scope, SymbolTreeVisitor, TokenHelper}
import scalafix.v1._

import scala.meta.{Import, Mod, Pat, Stat}

/**
  * A rule that removes unreferenced classes,
  * needs to be run after Analysis
  */
class DeadCodeRemover(model: ProjectModel, debug: Boolean) extends AbstractRule("ScalaCleanDeadCodeRemover", model, debug) {

  type Colour = Usage

  sealed trait Purpose {
    def id: Int

    override def toString: String = getClass.getSimpleName.replace("$", "")
  }

  override def debugDump(): Unit = {
    println("-------------------------------------------------------------")

    val used = model.allOf[ModelElement].filter(!_.colour.isUnused).toList.map(_.legacySymbol).sortBy(_.toString())
    val unused = model.allOf[ModelElement].filter(_.colour.isUnused).toList.map(_.legacySymbol).sortBy(_.toString())

    println("Used symbols =  " + used.size)
    println("Unused size = " + unused.size)
    println("Used Elements: ")
    used foreach (e => println("  " + e))
    println("Unused Elements: ")
    unused foreach (e => println("  " + e))
    println("-------------------------------------------------------------")

  }


  object Main extends Purpose {
    override def id: Int = 1
  }

  object Test extends Purpose {
    override def id: Int = 1 << 1
  }

  object Usage {
    private val usages = Array.tabulate(3) {
      Usage(_)
    }
    val unused = usages(0)
  }

  case class Usage(purposes: Int) extends Mark {
    def withPurpose(purpose: Purpose): Usage = {
      Usage.usages(purposes | purpose.id)
    }

    def hasPurpose(purpose: Purpose): Boolean =
      0 != (purposes & purpose.id)

    def isUnused = {
      purposes == 0
    }
  }

  override def markInitial = {
    markAll[ModelElement](Usage.unused)
  }


  def markUsed(element: ModelElement, markEnclosing: Boolean, purpose: Purpose, path: List[ModelElement], comment: String): Unit = {
    def markRhs(element: ModelElement, path: List[ModelElement], comment: String): Unit = {
      element.fields foreach {
        case valDef: ValModel => if (!valDef.isLazy) {
          valDef.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, true, purpose, valDef :: path, s"$comment -> valDef(outgoing)")
          }
          markRhs(valDef, valDef :: path, s"$comment -> valDef")
        }
        case varDef: VarModel =>
          varDef.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, true, purpose, varDef :: path, s"$comment -> varDef(outgoing)")
          }
          markRhs(varDef, varDef :: path, s"$comment -> varDef")
        //TODO - not sure if this is correct
        // an inner object is lazy in scala, so probably should only be marked when used
        case obj: ObjectModel =>
          obj.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, true, purpose, obj :: path, s"$comment -> obj(outgoing)")
          }
          markRhs(obj, obj :: path, s"$comment -> obj")
      }
    }

    val current = element.colour

    if (!current.hasPurpose(purpose)) {
      println(s"mark ${element} as used for $purpose due to ${path.mkString("->")} $comment")

      element.colour = current.withPurpose(purpose)
      //all the elements that this refers to
      element.internalOutgoingReferences foreach {
        case (ref, _) => markUsed(ref, true, purpose, element :: path, s"$comment -> internalOutgoingReferences")
      }

      // for the vars, (non lazy) vals and objects - eagerly traverse the RHS, as it is called
      // and the RHS will be executed
      // (its reallity) even if we later remove the field
      // we could consider marking at as used differently - a different colour
      //
      // don't mark the fields as used though
      markRhs(element, element :: path, s"$comment -> markRhs")

      //enclosing
      element.enclosing foreach {
        enclosed => markUsed(enclosed, true, purpose, element :: path, s"$comment - enclosing")
      }

      //overridden
      element.internalTransitiveOverrides foreach {
        enclosed => markUsed(enclosed, true, purpose, element :: path, s"$comment - overrides")
      }

      //overrides
      element.internalTransitiveOverriddenBy foreach {
        enclosed => markUsed(enclosed, false, purpose, element :: path, s"$comment - overrides")
      }
      element match {
        case getter:GetterMethodModel =>
          getter.field foreach {
            f => markUsed (f, true, purpose, element :: path, s"$comment - field ")
          }
        case setter:SetterMethodModel =>
          setter.field foreach {
            f => markUsed (f, true, purpose, element :: path, s"$comment - field ")
          }
        case _ =>
      }
    }
  }


  override def runRule(): Unit = {
    allMainEntryPoints foreach (e => markUsed(e, true, Main, e :: Nil, ""))
  }

  override def printSummary: Unit =
    println(s"""
       |linesRemoved    = $linesRemoved
       |linesChanged    = $linesChanged
       |elementsRemoved = $elementsRemoved
       |elementsVisited = $elementsVisited
       |""".stripMargin)

  var linesRemoved = 0
  var linesChanged = 0
  var elementsRemoved = 0
  var elementsVisited = 0

  override def fix(implicit doc: SemanticDocument): Patch = {

    val tv = new SymbolTreeVisitor {

      override protected def handlerSymbol(
                                            symbol: ElementId, mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        if(symbol.symbol.isLocal || symbol.symbol.isNone) continue
        else {
          val modelElementOpt = model.getLegacySymbol[ModelElement](symbol)
          modelElementOpt match {
            case None =>
              continue
            case Some(modelElement) =>

              if (modelElement.existsInSource) {
                val usage = modelElement.colour
                elementsVisited += 1
                if (usage.isUnused) {
                  val tokens = stat.tokens
                  val firstToken = tokens.head

                  val removedTokens = TokenHelper.whitespaceOrCommentsBefore(firstToken, doc.tokens) ++ tokens
                  elementsRemoved += 1
                  val first = removedTokens.minBy {
                    _.start
                  }
                  linesRemoved += (stat.pos.endLine - first.pos.startLine + 1)
                  val patch = Patch.removeTokens(removedTokens)
                  (patch, false)
                } else
                  continue
              } else
                continue
          }
        }
      }

      override protected def handlerPats(
                                          pats: Seq[Pat.Var], mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        elementsVisited += 1
        val declarationsByUsage: Map[Usage, Seq[(Pat.Var, ModelElement)]] =
          pats.filterNot(v => v.symbol.isLocal  || v.symbol.isNone ) map { p =>
            println(p.symbol)
            println(ElementId(p.symbol))
            val mElement: ModelElement = model.legacySymbol[ModelElement](ElementId(p.symbol))
            (p, mElement)
          } groupBy (m => m._2.colour)

        declarationsByUsage.get(Usage.unused) match {
          case Some(_) if declarationsByUsage.size == 1 =>
            //we can remove the whole declaration
            val tokens = stat.tokens
            val firstToken = tokens.head
            val removedTokens = TokenHelper.whitespaceOrCommentsBefore(firstToken, doc.tokens) ++ tokens
            elementsRemoved += 1
            val first = removedTokens.minBy{ _.start}
            linesRemoved += (stat.pos.endLine - first.pos.startLine + 1)
            (Patch.removeTokens(removedTokens), false)
          case Some(unused) =>
            val combinedPatch = unused.foldLeft(Patch.empty) {
              case (patch, (pat, model)) =>
                patch + Patch.replaceToken(pat.tokens.head, "_")
            }
            val marker = Utils.addMarker(stat, s"consider rewriting pattern as ${unused.size} values are not used")
            linesChanged += 1
            (combinedPatch + marker, true)
          case _ =>
            continue

        }
      }

      override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = {
        (Patch.empty, false)
      }
//      override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = {
//        assert(importStatement.importers.size == 1)
//        //TODO - need to ensure that all symbols related are the same import are of the same status
//
//        val importers = importStatement.importers
//        val importees = importers.head.importees
//        val byUsage = importees.groupBy {
//          i =>
//            val symbol = i.symbol(doc)
//            model.getLegacySymbol[ModelElement](ElementId(symbol)).map(_.colour)
//        }
//        byUsage.get(Some(Usage.unused)) match {
//          case Some(_) if byUsage.size == 1 =>
//            //we can remove the whole declaration
//            val tokens = importStatement.tokens
//            val firstToken = tokens.head
//            (Patch.removeTokens(TokenHelper.whitespaceTokensBefore(firstToken, doc.tokens)) + Patch.removeTokens(tokens), false)
//          case Some(unused) =>
//            val combinedPatch = unused.foldLeft(Patch.empty) {
//              case (patch, importee) =>
//                patch + Patch.removeImportee(importee)
//            }
//            (combinedPatch, false)
//          case _ =>
//            (Patch.empty, false)
//        }
//      }
    }
    tv.visitDocument(doc.tree)
  }
}
