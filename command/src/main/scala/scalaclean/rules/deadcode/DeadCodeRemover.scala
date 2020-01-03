package scalaclean.rules.deadcode

import scalaclean.model._
import scalaclean.rules.AbstractRule
import scalaclean.util.{ElementTreeVisitor, TokenHelper}
import scalafix.v1._

import scala.meta.io.AbsolutePath

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
//    println("-------------------------------------------------------------")
//
//    val used = model.allOf[ModelElement].filter(!_.colour.isUnused).toList.map(_.legacySymbol).sortBy(_.toString())
//    val unused = model.allOf[ModelElement].filter(_.colour.isUnused).toList.map(_.legacySymbol).sortBy(_.toString())
//
//    println("Used symbols =  " + used.size)
//    println("Unused size = " + unused.size)
//    println("Used Elements: ")
//    used foreach (e => println("  " + e))
//    println("Unused Elements: ")
//    unused foreach (e => println("  " + e))
//    println("-------------------------------------------------------------")
//
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

  override def fix(targetFile: AbsolutePath, syntacticDocument: SyntacticDocument)(implicit semanticDocument: SemanticDocument): List[SCPatch] = {

    val targetFileName = targetFile.toString
    // find source model
    val sModel = model.allOf[SourceModel].filter(_.toString.contains(targetFileName)).toList.headOption.getOrElse(throw new IllegalStateException(s"Unable to find source model for $targetFileName"))

    val tokens = syntacticDocument.tokens.tokens

    val visitor: ElementTreeVisitor = new ElementTreeVisitor {

      def remove(element: ModelElement, comment: String = ""): Unit = {
        replace(element, "", "remove", comment)
      }

      def replace(element: ModelElement, text: String, actionName: String = "replace", comment: String = ""): Unit = {
        log(s" $actionName(${element.name},'$text')   isUnused = ${element.colour.isUnused}")
        log(" ------- ")
        log("  cm.rawStart = " + element.rawStart)
        val start = element.annotations.map(a => element.rawStart + a.posOffsetStart - 1).headOption.getOrElse(element.rawStart)
        log("  annotStart = " + start)
        val candidateBeginToken = tokens.find(t => t.start >= start && t.start <= t.end).head
        log("  annotStart = " + candidateBeginToken)
        val newBeingToken = TokenHelper.whitespaceOrCommentsBefore(candidateBeginToken, syntacticDocument.tokens)
//        log("  newBeginToken = " + newBeingToken)
        val newStartPos = newBeingToken.headOption.map(_.start).getOrElse(start)
        log("  newStartPos = " + newStartPos + " -> " + element.rawEnd)

        collect(SCPatch(newStartPos, element.rawEnd, text, comment))
      }

      def replaceFromFocus(element: ModelElement, text: String, comment: String): Unit = {
        log(s" replaceFromFocus(${element.name},'$text')  ${element.rawFocusStart}->${element.rawEnd}")
        collect(SCPatch(element.rawFocusStart, element.rawEnd, text, comment))
      }

      def addComment(element: ModelElement, msg: String, comment: String = ""): Unit = {
        log(" addComment(" + element.name + ",'" + msg + "')")
        val text = s"/* *** SCALA CLEAN $msg */"
        collect(SCPatch(element.rawStart, element.rawStart, text, comment))
      }

      override protected def visitSymbol(element: ModelElement): Boolean = {
        element match {
          case _ if !element.existsInSource =>
            true
          case field: FieldModel if field.inCompoundFieldDeclaration =>
            // do nothing - do not recurse
            false
          case gmm: GetterMethodModel if(gmm.field.forall(_.existsInSource)) =>

            false
          case fields: FieldsModel =>
            log("FieldsModel - " + fields.name)
            // fields are a bit special. They will be marked as used (by the implementation of the var/val that uses it)
            // but we take a deeper look here - there are 3 cases
            // 1. all of the child fields are used - leave as is
            // 2. None of the fields are used - remove the whole declaration
            // 3. some of the fields are used - replace the unused fields with `-` and leave a comment
            val decls = fields.fieldsInDeclaration
            assert(decls.nonEmpty)
            val unused = decls.filter {
              _.colour.isUnused
            }
            if (unused.isEmpty) {
              //case 1 no change
              true
            } else if (unused.size == decls.size) {
              //case 2
              remove(fields, "all fields in patmat unused")
              //no need to recurse
              false
            } else {
              //case 3
              addComment(fields, s"consider rewriting pattern as ${unused.size} values are not used", "mutiple fields unused")

              unused foreach { f =>
                replaceFromFocus(f, "_",s"${f.name} unused in patmat")
              }
              true
            }

          case element =>
            log(" basic element handling")
            if (element.colour.isUnused && element.existsInSource) {
              remove(element, s"Simple ${element.name} (${element.getClass} unused")
              false
            }
            else
              true
        }
      }
    }

    visitor.visit(sModel)

    val result = visitor.result.toList.sortBy(_.startPos)
    println("--------NEW----------")
    result.foreach(println)
    println("------------------")


    result.map(s => SCPatch(s.startPos, s.endPos, s.replacementText))
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
