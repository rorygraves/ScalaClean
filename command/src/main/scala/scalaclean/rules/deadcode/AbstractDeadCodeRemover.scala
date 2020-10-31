package scalaclean.rules.deadcode

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.{RuleRun, SourceFile}
import scalaclean.util.{ScalaCleanTreePatcher, SingleFileVisit}

/** A rule that removes unreferenced classes */
abstract class AbstractDeadCodeRemover[T <: AbstractDeadCodeCommandLine] extends RuleRun[T] {

  type SpecificColour = Usage

  sealed trait Purpose {
    def id: Int

    override def toString: String = getClass.getSimpleName.replace("$", "")
  }

  override def debugDump(): Unit = {
    println("-------------------------------------------------------------")

    val used   = model.allOf[ModelElement].filter(_.colour.specific.isDefined).toList.map(_.modelElementId.id).sorted
    val unused = model.allOf[ModelElement].filter(_.colour.isInitial).toList.map(_.modelElementId.id).sorted
    val banned = model.allOf[ModelElement].filter(_.colour.changesAreBanned).toList.map(_.modelElementId.id).sorted

    println("Used symbols =  " + used.size)
    println("Unused size = " + unused.size)
    println("Banned size = " + banned.size)
    println("Used Elements: ")
    used.foreach(e => println("  " + e))
    println("Unused Elements: ")
    unused.foreach(e => println("  " + e))
    println("-------------------------------------------------------------")

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

  def markUsed(
      element: ModelElement,
      markEnclosing: Boolean,
      purpose: Purpose,
      path: List[ModelElement],
      comment: String
  ): Unit = {
    val current = element.colour
    if (!current.changesAreBanned && !current.specific.exists(_.hasPurpose(purpose))) {
      if (debug)
        println(s"mark $element as used for $purpose due to ${path.mkString("->")} $comment")

      val newSpecificUsage = current.specific.getOrElse(Usage(purpose)).withPurpose(purpose)
      element.colour = current.withSpecific(newSpecificUsage)
      adjustUsage(element, markEnclosing, purpose, path, comment)
    }
  }

  protected def adjustUsage(
      element: ModelElement,
      markEnclosing: Boolean,
      purpose: Purpose,
      path: List[ModelElement],
      comment: String
  ): Unit = {
    //all the elements that this refers to
    element.internalOutgoingReferences.foreach { case (ref, _) =>
      markUsed(ref, markEnclosing = true, purpose, element :: path, s"$comment -> internalOutgoingReferences")
    }

    // for the vars, (non lazy) vals and objects - eagerly traverse the RHS, as it is called
    // as the RHS will be executed
    // (its reality) even if we later remove the field
    // we could consider marking at as used differently - a different colour
    //
    // don't mark the fields as used though
    markRhs(element, purpose, element :: path, s"$comment -> markRhs")

    if (markEnclosing) {
      //enclosing
      element.enclosing.foreach { enclosed =>
        markUsed(enclosed, markEnclosing = true, purpose, element :: path, s"$comment - enclosing")
      }
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
    runBasicRule()
    runExtraRules()
    runSerialisationRule()
  }
  def runBasicRule(): Unit = {
    for (source <- model.allOf[SourceModel];
    if sourceMetaData(source).external) {
//TODO

    }

    allMainEntryPoints.foreach(e => markUsed(e, markEnclosing = true, Main, e :: Nil, "app"))
    allJunitTest.foreach(e => markUsed(e, markEnclosing = true, Test, e :: Nil, "junit test"))
    allJunitClasses.foreach { testClass =>
      markUsed(testClass, markEnclosing = true, Test, testClass :: Nil, "junit test class")
    }
    allScalaTests.foreach(e => markUsed(e, markEnclosing = true, Test, e :: Nil, "scalatests"))
    //we dont really want to do this but we don't successfully remove parameters,
    // and classes parameters overlap with the val
    model.allOf[FieldModel].filter(_.isParameter) foreach { e=>
      e.mark = Mark.dontChange(SimpleReason("parameter") )
    }
    model.allOf[FieldModel].filter(_.associatedConstructorParameter.isDefined) foreach { e=>
      e.mark = Mark.dontChange(SimpleReason("has ctor val") )
      e.associatedConstructorParameter.get.enclosing.headOption.foreach {
        _.mark = Mark.dontChange(SimpleReason("is ctor") )
      }
    }
    model.allOf[PlainMethodModel].filter(_.defaultAccessorFor.isDefined) foreach { e=>
      e.mark = Mark.dontChange(SimpleReason("default accessor methods") )
    }
    model.allOf[PlainMethodModel].filter{
      case method: PlainMethodModel =>
      method.methodName == "<init>"
    } foreach { e=>
      e.mark = Mark.dontChange(SimpleReason("constructor") )
    }
    for (cls <- model.allOf[ClassLike];
         self <- cls.selfType) {
      self.mark = Mark.dontChange(SimpleReason("self type field") )
    }
  }
  def runExtraRules(): Unit = {}
  def runSerialisationRule(): Unit = {
    allSerialisationEntries.foreach(e =>
    //we don't really want to mark a class as used just because it had a serialisation method
    //so we limit it to just those that seem to be in use already
    //you could also consider that all serialisable classes are live ....
      if (!e.classOrEnclosing.colour.isInitial)
        markUsed(e, markEnclosing = false, Main, e :: Nil, "serialisationCode"))
  }

  override def generateFixes(sourceFile: SourceFile): SingleFileVisit = {

    object visitor extends ScalaCleanTreePatcher(sourceFile, AbstractDeadCodeRemover.this.options) {

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
              _.colour.isInitial
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
            if (element.colour.isInitial && element.existsInSource) {
              remove(element, s"Simple ${element.name} (${element.getClass} unused")
              false
            } else
              true
        }
      }
    }

    visitor.visit(sourceFile.file)

    visitor.result
  }

  object Usage {
    def apply (p: Purpose) = new Usage(p.id)
  }
  case class Usage private (existingPurposes: Int) extends SomeSpecificColour {
    require (existingPurposes != 0)
    override type RealType = Usage

    override def merge(other: Usage): Usage = {
      val all = this.existingPurposes | other.existingPurposes
      if (all == this.existingPurposes) this
      else if (all == other.existingPurposes) other
      else Usage(all)
    }

    def withPurpose(addedPurpose: Purpose): Usage = {
      if (hasPurpose(addedPurpose)) this
      else Usage(existingPurposes | addedPurpose.id)
    }

    def hasPurpose(purpose: Purpose): Boolean = 0 != (existingPurposes & purpose.id)

  }

  object Main extends Purpose {
    override def id: Int = 1
  }

  object Test extends Purpose {
    override def id: Int = 1 << 1
  }
  val mainUsage = Usage(Main)
  val testUsage = Usage(Test)

}
class AbstractDeadCodeCommandLine extends ScalaCleanCommandLine
