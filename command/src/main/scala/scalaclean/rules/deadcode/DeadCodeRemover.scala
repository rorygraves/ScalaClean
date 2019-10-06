package scalaclean.rules.deadcode

import scalaclean.model._
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

    val used = model.allOf[ModelElement].filter(!_.colour.isUnused).toList.map(_.symbol).sortBy(_.toString())
    val unused = model.allOf[ModelElement].filter(_.colour.isUnused).toList.map(_.symbol).sortBy(_.toString())

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

  def allMainEntryPoints = {
    allMainMethodEntries ++ allApp
  }

  def allMainMethodEntries = {
    val stringArray = List(List(Symbol))
    (for (obj <- model.allOf[ObjectModel] if (obj.enclosing.isEmpty);
          method <- obj.methods if method.name == "main") //&& method.paramsType = stringArray
      yield {
        List(method, obj)
      }).flatten
  }

  def allApp = {
    for (obj <- model.allOf[ObjectModel] if (obj.xtends(Symbol("G:scala/App#"))))
      yield obj
  }

  //  def allTestEntryPoints = {
  //    allMainMethodEntries ++ allApp
  //  }
  //  def allJunitTest = {
  //    model.allOf[MethodModel] collect {
  //      case method if (method.annotations)
  //    }
  //  }

  def markUsed(element: ModelElement, purpose: Purpose, path: List[ModelElement]): Unit = {
    def markRhs(element: ModelElement, path: List[ModelElement]): Unit = {
      element.fields foreach {
        case valDef: ValModel => if (!valDef.isLazy) {
          valDef.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, purpose, valDef :: path)
          }
          markRhs(valDef, valDef :: path)
        }
        case varDef: VarModel =>
          varDef.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, purpose, varDef :: path)
          }
          markRhs(varDef, varDef :: path)
        case obj: ObjectModel =>
          obj.internalOutgoingReferences foreach {
            case (ref, _) => markUsed(ref, purpose, obj :: path)
          }
          markRhs(obj, obj :: path)
        //remove this after migration
        case _: impl.hooks.FieldModelHook => ???
      }
    }

    val current = element.colour

    if (!current.hasPurpose(purpose)) {
      println(s"mark ${element} as used for $purpose due to ${path.mkString("->")}")

      element.colour = current.withPurpose(purpose)
      element.internalOutgoingReferences foreach {
        case (ref, _) => markUsed(ref, purpose, element :: path)
      }
      markRhs(element, element :: path)
      //TODO for the vars, (non lazy) vals and objects - eagerly traverse

      //TODO consider marking the overridded and overrides
      //TODO consider marking the inherited classes
      //TODO consider marking the enclosing like markUsed(element.owner)
    }
  }


  override def runRule(): Unit = {
    allMainEntryPoints foreach (e => markUsed(e, Main, e :: Nil))
  }

  override def fix(implicit doc: SemanticDocument): Patch = {

    val tv = new SymbolTreeVisitor {

      override protected def handlerSymbol(
        symbol: Symbol, mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        val modelElement = model.fromSymbol[ModelElement](symbol)
        val usage = modelElement.colour
        if (usage.isUnused) {
          val tokens = stat.tokens
          val firstToken = tokens.head

          val removedTokens = TokenHelper.whitespaceOrCommentsBefore(firstToken, doc.tokens) ++ tokens
          val patch = Patch.removeTokens(removedTokens)
          (patch, false)
        } else
          continue
      }

      override protected def handlerPats(
        pats: Seq[Pat.Var], mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {

        val declarationsByUsage: Map[Usage, Seq[(Pat.Var, ModelElement)]] =
          pats map { p =>
//            println(" p . pos = " + stat.pos.start, stat.pos.end)
            (p, model.fromSymbolLocal[ModelElement](p.symbol, stat.pos.start, stat.pos.end))
          } groupBy (m => m._2.colour)

        declarationsByUsage.get(Usage.unused) match {
          case Some(_) if declarationsByUsage.size == 1 =>
            //we can remove the whole declaration
            val tokens = stat.tokens
            val firstToken = tokens.head
            (Patch.removeTokens(TokenHelper.whitespaceOrCommentsBefore(firstToken, doc.tokens)) + Patch.removeTokens(tokens), false)
          case Some(unused) =>
            val combinedPatch = unused.foldLeft(Patch.empty) {
              case (patch, (pat, model)) =>
                patch + Patch.replaceToken(pat.tokens.head, "_")
            }
            val marker = Utils.addMarker(stat, s"consider rewriting pattern as ${unused.size} values are not used")
            (combinedPatch + marker, true)
          case _ =>
            continue

        }
      }

      override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = {
        assert(importStatement.importers.size == 1)
        //TODO - need to ensure that all symbols related are the same import are of the same status

        val importers = importStatement.importers
        val importees = importers.head.importees
        val byUsage = importees.groupBy {
          i =>
            val symbol = i.symbol(doc)
            model.getSymbol[ModelElement](symbol).map(_.colour)
        }
        byUsage.get(Some(Usage.unused)) match {
          case Some(_) if byUsage.size == 1 =>
            //we can remove the whole declaration
            val tokens = importStatement.tokens
            val firstToken = tokens.head
            (Patch.removeTokens(TokenHelper.whitespaceTokensBefore(firstToken, doc.tokens)) + Patch.removeTokens(tokens), false)
          case Some(unused) =>
            val combinedPatch = unused.foldLeft(Patch.empty) {
              case (patch, importee) =>
                patch + Patch.removeImportee(importee)
            }
            (combinedPatch, false)
          case _ =>
            (Patch.empty, false)
        }
      }
    }
    tv.visitDocument(doc.tree)
  }
}
