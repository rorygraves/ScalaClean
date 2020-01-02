package scalaclean.rules.deadcode

import scalaclean.model._
import scalaclean.model.impl.{ElementId, ElementModelImpl}
import scalaclean.rules.AbstractRule
import scalaclean.util.{ElementTreeVisitor, Scope, SymbolTreeVisitor, TokenHelper}
import scalafix.v1._

import scala.collection.mutable.ListBuffer
import scala.meta.io.AbsolutePath
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


  override def markInitial(): Unit = {
    markAll[ModelElement](Usage.unused)
  }

  def markUsed(element: ModelElement, markEnclosing: Boolean, purpose: Purpose, path: List[ModelElement], comment: String): Unit = {
    def markRhs(element: ModelElement, path: List[ModelElement], comment: String): Unit = {
      element.fields foreach {
        case valDef: ValModel => if (!valDef.isLazy) {
          valDef.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, markEnclosing = true, purpose, valDef :: path, s"$comment -> valDef(outgoing)")
          }
          markRhs(valDef, valDef :: path, s"$comment -> valDef")
        }
        case varDef: VarModel =>
          varDef.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, markEnclosing = true, purpose, varDef :: path, s"$comment -> varDef(outgoing)")
          }
          markRhs(varDef, varDef :: path, s"$comment -> varDef")
        //TODO - not sure if this is correct
        // an inner object is lazy in scala, so probably should only be marked when used
        case obj: ObjectModel =>
          obj.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, markEnclosing = true, purpose, obj :: path, s"$comment -> obj(outgoing)")
          }
          markRhs(obj, obj :: path, s"$comment -> obj")
      }
    }

    val current = element.colour

    if (!current.hasPurpose(purpose)) {
      println(s"mark $element as used for $purpose due to ${path.mkString("->")} $comment")

      element.colour = current.withPurpose(purpose)
      //all the elements that this refers to
      element.internalOutgoingReferences foreach {
        case (ref, _) => markUsed(ref, markEnclosing = true, purpose, element :: path, s"$comment -> internalOutgoingReferences")
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
        enclosed => markUsed(enclosed, markEnclosing = true, purpose, element :: path, s"$comment - enclosing")
      }

      //overridden
      element.internalTransitiveOverrides foreach {
        enclosed => markUsed(enclosed, markEnclosing = true, purpose, element :: path, s"$comment - overrides")
      }

      //overrides
      element.internalTransitiveOverriddenBy foreach {
        enclosed => markUsed(enclosed, markEnclosing = false, purpose, element :: path, s"$comment - overrides")
      }
      element match {
        case accessor: AccessorModel =>
          accessor.field foreach {
            f => markUsed(f, markEnclosing = true, purpose, element :: path, s"$comment - field ")
          }
        case _ =>
      }
    }
  }


  override def runRule(): Unit = {
    allMainEntryPoints foreach (e => markUsed(e, markEnclosing = true, Main, e :: Nil, ""))
    allJunitTest foreach (e => markUsed(e, markEnclosing = true, Test, e :: Nil, ""))
  }

  override def printSummary(projectName: String): Unit =
    println(
      s"""
         |Project name    = $projectName
         |linesRemoved    = $linesRemoved
         |linesChanged    = $linesChanged
         |elementsRemoved = $elementsRemoved
         |elementsVisited = $elementsVisited
         |""".stripMargin)

  var linesRemoved = 0
  var linesChanged = 0
  var elementsRemoved = 0
  var elementsVisited = 0


  override def fix(targetFile: AbsolutePath, syntacticDocument: SyntacticDocument)(implicit semanticDocument: SemanticDocument): List[(Int, Int, String)] = {

    val targetFileName = targetFile.toString
    // find source model
    val sModel = model.allOf[SourceModel].filter(_.toString.contains(targetFileName)).toList.headOption.getOrElse(throw new IllegalStateException(s"Unable to find source model for $targetFileName"))


    val tokens = syntacticDocument.tokens.tokens

    val topLevelElements = sModel.innerClassLike.flatMap { cm =>
      if(cm.existsInSource) {
        Some(cm)
      }else
        None
    }.sortBy(_.rawStart)



    val lb2 = new ListBuffer[(Int, Int, String)]()
    def recurse(element: ElementModelImpl): List[(Int, Int, String)] = {
      val usage = element.colour
      if(element.existsInSource && usage.isUnused) {
        println(element.name + "  " + usage.isUnused)
        println("-- ")
        println("  cm.rawStart = " + element.rawStart)
        val start = element.annotations.map(a => element.rawStart + a.posOffsetStart -1).headOption.getOrElse(element.rawStart)
        println("  annotStart = " + start)
        val candidateBeginToken = tokens.find(t => t.start >= start && t.start <= t.end).head
        println("  annotStart = " + candidateBeginToken)
        val newBeingToken = TokenHelper.whitespaceOrCommentsBefore(candidateBeginToken, syntacticDocument.tokens)
        println("  newBeginToken = " + newBeingToken)
        val newStartPos = newBeingToken.headOption.map(_.start).getOrElse(start)
        println("  newStartPos = " + newStartPos)

        lb2.append((newStartPos, element.rawEnd, ""))
        List((newStartPos, element.rawEnd, ""))

      } else {
        element.allChildren.flatMap(recurse)
      }
    }
    val toDelete = sModel.allChildren.flatMap { element => recurse(element) }

    println("--------NEW----------")
    lb2.toList.sortBy(_._1).foreach(println)
    println("------------------")


    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    val visitor = new ElementTreeVisitor[(Int,Int,String)] {
      override def visitClass(cm: ClassModel): Boolean = {
        if(cm.existsInSource && cm.colour.isUnused) {
          log(" **Removing** ")
          false
        } else
          true

      }
    }
    visitor.visit(sModel)


    return lb2.toList.sortBy(_._1)
    val lb = new ListBuffer[(Int, Int, String)]()
    val tv = new SymbolTreeVisitor {

      override protected def handlerSymbol(
                                            symbol: ElementId, mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        if (symbol.symbol.isLocal || symbol.symbol.isNone) continue
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

                  val removedTokens = TokenHelper.whitespaceOrCommentsBefore(firstToken, semanticDocument.tokens) ++ tokens
                  elementsRemoved += 1
                  val first = removedTokens.minBy {
                    _.start
                  }
                  linesRemoved += (stat.pos.endLine - first.pos.startLine + 1)

                  val startPos = first.pos.start
                  val endPos = stat.pos.end
                  println("StartPos = " + startPos + "  ->  " + endPos + "=> \"\"")
                  lb.append((startPos, endPos, ""))
                  (Patch.removeTokens(removedTokens), false)
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
          pats.filterNot(v => v.symbol.isLocal || v.symbol.isNone) map { p =>
            val mElement: ModelElement = model.legacySymbol[ModelElement](ElementId(p.symbol))
            (p, mElement)
          } groupBy (m => m._2.colour)

        declarationsByUsage.get(Usage.unused) match {
          case Some(_) if declarationsByUsage.size == 1 =>
            //we can remove the whole declaration
            val tokens = stat.tokens
            val firstToken = tokens.head
            val removedTokens = TokenHelper.whitespaceOrCommentsBefore(firstToken, semanticDocument.tokens) ++ tokens
            elementsRemoved += 1
            val first = removedTokens.minBy {
              _.start
            }
            linesRemoved += (stat.pos.endLine - first.pos.startLine + 1)

            val startPos = first.pos.start
            val endPos = stat.pos.end
            println("StartPos = " + startPos + "  ->  " + endPos + "=> \"\"")
            lb.append((startPos, endPos, ""))
            (Patch.removeTokens(removedTokens), false)
          case Some(unused) =>
            val combinedPatch = unused.foldLeft(Patch.empty) {
              case (patch, (pat, _)) =>
                val token = pat.tokens.head
                val endPos = token.end
                val startPos = token.start
                println("StartPos = " + startPos + "  ->  " + endPos + "=> \"_\"")
                lb.append((startPos, endPos, "_"))
                patch + Patch.replaceToken(pat.tokens.head, "_")
            }
            val marker = Utils.addMarker(stat, s"consider rewriting pattern as ${unused.size} values are not used")
            Utils.addMarker2(stat, s"consider rewriting pattern as ${unused.size} values are not used").foreach(lb.append(_))
            linesChanged += 1
            (combinedPatch + marker, true)
          case _ =>
            continue

        }
      }

    }
    tv.visitDocument(semanticDocument.tree)

    lb.toList.sortBy(_._1)
  }


  case class Usage(existingPurposes: Int = 0) extends Mark {
    def withPurpose(addedPurpose: Purpose): Usage = {
      Usage(existingPurposes | addedPurpose.id)
    }

    def hasPurpose(purpose: Purpose): Boolean =
      0 != (existingPurposes & purpose.id)

    def isUnused: Boolean = {
      existingPurposes == 0
    }
  }

  object Main extends Purpose {
    override def id: Int = 1
  }

  object Test extends Purpose {
    override def id: Int = 1 << 1
  }

  object Usage {
    val unused: Usage = new Usage(0)
  }

}
