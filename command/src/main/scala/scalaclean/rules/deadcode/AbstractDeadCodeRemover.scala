package scalaclean.rules.deadcode

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.RuleRun
import scalaclean.util.ScalaCleanTreePatcher
import scalafix.v1.SyntacticDocument

import scala.meta.io.AbsolutePath

/** A rule that removes unreferenced classes */
abstract class AbstractDeadCodeRemover[T <: AbstractDeadCodeCommandLine] extends RuleRun[T] {

  type Colour = Usage

  sealed trait Purpose {
    def id: Int

    override def toString: String = getClass.getSimpleName.replace("$", "")
  }

  override def debugDump(): Unit = {
    println("-------------------------------------------------------------")

    val used   = model.allOf[ModelElement].filter(!_.colour.isUnused).toList.map(_.modelElementId.id).sorted
    val unused = model.allOf[ModelElement].filter(_.colour.isUnused).toList.map(_.modelElementId.id).sorted

    println("Used symbols =  " + used.size)
    println("Unused size = " + unused.size)
    println("Used Elements: ")
    used.foreach(e => println("  " + e))
    println("Unused Elements: ")
    unused.foreach(e => println("  " + e))
    println("-------------------------------------------------------------")

  }

  override def markInitial(): Unit = {
    markAll[ModelElement](Usage.unused)
  }

  def markRhs(element: ModelElement, purpose: Purpose, path: List[ModelElement], comment: String): Unit = {
    element.fields.foreach {
      case valDef: ValModel =>
        if (!valDef.isLazy) {
          valDef.internalOutgoingReferences.foreach { case (ref, _) =>
            markUsed(ref, markEnclosing = true, purpose, valDef :: path, s"$comment -> valDef(outgoing)")
          }
          markRhs(valDef, purpose, valDef :: path, s"$comment -> valDef")
        }
      case varDef: VarModel =>
        varDef.internalOutgoingReferences.foreach { case (ref, _) =>
          markUsed(ref, markEnclosing = true, purpose, varDef :: path, s"$comment -> varDef(outgoing)")
        }
        markRhs(varDef, purpose, varDef :: path, s"$comment -> varDef")
      //TODO - not sure if this is correct
      // an inner object is lazy in scala, so probably should only be marked when used
      case obj: ObjectModel =>
        obj.internalOutgoingReferences.foreach { case (ref, _) =>
          markUsed(ref, markEnclosing = true, purpose, obj :: path, s"$comment -> obj(outgoing)")
        }
        markRhs(obj, purpose, obj :: path, s"$comment -> obj")
    }
  }

  def markIndirectReferences = true

  def markUsed(
      element: ModelElement,
      markEnclosing: Boolean,
      purpose: Purpose,
      path: List[ModelElement],
      comment: String
  ): Unit = {
    val current = element.colour
    if (!current.hasPurpose(purpose)) {
      if (debug) {
        println(s"mark $element as used for $purpose due to ${path.mkString("->")} $comment")
      }

      element.colour = current.withPurpose(purpose)
      adjustUsage(element, purpose, path, comment, current)
    }
  }

  protected def adjustUsage(
      element: ModelElement,
      purpose: Purpose,
      path: List[ModelElement],
      comment: String,
      current: Usage
  ): Unit = {
    if (markIndirectReferences) {
      //all the elements that this refers to
      element.internalOutgoingReferences.foreach { case (ref, _) =>
        markUsed(ref, markEnclosing = true, purpose, element :: path, s"$comment -> internalOutgoingReferences")
      }

      // for the vars, (non lazy) vals and objects - eagerly traverse the RHS, as it is called
      // and the RHS will be executed
      // (its reality) even if we later remove the field
      // we could consider marking at as used differently - a different colour
      //
      // don't mark the fields as used though
      markRhs(element, purpose, element :: path, s"$comment -> markRhs")
    }

    //enclosing
    element.enclosing.foreach { enclosed =>
      markUsed(enclosed, markEnclosing = true, purpose, element :: path, s"$comment - enclosing")
    }

    //overridden
    element.internalTransitiveOverrides.foreach { enclosed =>
      markUsed(enclosed, markEnclosing = true, purpose, element :: path, s"$comment - overrides")
    }

    //overrides
    element.internalTransitiveOverriddenBy.foreach { enclosed =>
      markUsed(enclosed, markEnclosing = false, purpose, element :: path, s"$comment - overrides")
    }
    element match {
      case accessor: AccessorModel =>
        accessor.field.foreach { f =>
          markUsed(f, markEnclosing = true, purpose, element :: path, s"$comment - field ")
        }
      case obj: ObjectModel =>
        //not sure if this is needed. Apply method should be referenced directly
        //is this a ScalaMeta hangover??
        obj.methods.foreach { m =>
          if (m.methodName == "apply")
            markUsed(m, markEnclosing = false, purpose, element :: path, s"$comment - apply method of used object")
        }
      case field: FieldModel =>
        field.declaredIn.foreach { f =>
          markUsed(f, markEnclosing = true, purpose, element :: path, s"$comment - fields declaration ")
        }
      //not sure if this is needed.
      //is this a ScalaMeta hangover??
      case trt: TraitModel =>
        trt.fields.foreach { fieldsInTrait =>
          markUsed(fieldsInTrait, markEnclosing = false, purpose, element :: path, s"$comment - inside a used trait")
        }
      case _ =>
    }
  }

  override def runRule(): Unit = {
    allMainEntryPoints.foreach(e => markUsed(e, markEnclosing = true, Main, e :: Nil, ""))
    allJunitTest.foreach(e => markUsed(e, markEnclosing = true, Test, e :: Nil, ""))
    allSerialisationEntries.foreach(e => markUsed(e, markEnclosing = false, Main, e :: Nil, "serialisationCode"))
  }

  override def fix(targetFile: AbsolutePath, syntacticDocument: () => SyntacticDocument): List[SCPatch] = {

    val targetFileName = targetFile.toString
    // find source model
    val sModel = model
      .allOf[SourceModel]
      .filter(_.toString.contains(targetFileName))
      .toList
      .headOption
      .getOrElse(throw new IllegalStateException(s"Unable to find source model for $targetFileName"))

    object visitor extends ScalaCleanTreePatcher(patchStats, syntacticDocument) {
      override def debug: Boolean       = options.debug
      override def addComments: Boolean = options.addComments

      override protected def visitInSource(element: ModelElement): Boolean = {
        element match {
          case field: FieldModel if field.inCompoundFieldDeclaration =>
            // do nothing - do not recurse
            false
          case gmm: GetterMethodModel if gmm.field.forall(_.existsInSource) =>
            false
          case fields: FieldsModel =>
            if (debug)
              log("FieldsModel - " + fields.name)
            // fields are a bit special. They will be marked as used (by the implementation of the var/val that uses it)
            // but we take a deeper look here - there are 3 cases
            // 1. all of the child fields are used - leave as is
            // 2. None of the fields are used - remove the whole declaration
            // 3. some of the fields are used - replace the unused fields with `-` and leave a comment
            val decls = fields.fieldsInDeclaration
//            assert(decls.nonEmpty)
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
              addComment(
                fields,
                s"consider rewriting pattern as ${unused.size} values are not used",
                "multiple fields unused"
              )

              unused.foreach(f => replaceFromFocus(f, "_", s"${f.name} unused in patmat"))
              true
            }

          case element =>
            if (debug)
              log(" basic element handling")
            if (element.colour.isUnused && element.existsInSource) {
              remove(element, s"Simple ${element.name} (${element.getClass} unused")
              false
            } else
              true
        }
      }
    }

    visitor.visit(sModel)

    val result = visitor.result
    if (debug) {
      println("--------NEW----------")
      result.foreach(println)
      println("------------------")
    }

    result//.map(s => SCPatch(s.startPos, s.endPos, s.replacementText))
  }

  case class Usage(existingPurposes: Int = 0) extends Mark {

    def withPurpose(addedPurpose: Purpose): Usage = {
      Usage(existingPurposes | addedPurpose.id)
    }

    def hasPurpose(purpose: Purpose): Boolean = 0 != (existingPurposes & purpose.id)

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
class AbstractDeadCodeCommandLine extends ScalaCleanCommandLine
