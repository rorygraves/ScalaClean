package scalaclean.rules.deadcode

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.{RuleRun, SourceFile}
import scalaclean.util.{ScalaCleanTreePatcher, SingleFileVisit}

import scala.collection.mutable

/** A rule that removes unreferenced classes */
abstract class AbstractDeadCodeRemover[T <: AbstractDeadCodeCommandLine] extends RuleRun[T] {

  type SpecificColour = Usage

  sealed trait Purpose {
    def id: Int

    def withClass: Purpose
    def withoutClass: Purpose

    override def toString: String = getClass.getSimpleName.replace("$", "")
  }

  override def debugDump(): Unit = {
    println("-------------------------------------------------------------")

    val used   = model.allOf[ModelElement].filter(_.colour.specific.isDefined).toList.sortBy(_.modelElementId.id)
    val unused = model.allOf[ModelElement].filter(_.colour.isInitial).toList.sortBy(_.modelElementId.id)
    val banned = model.allOf[ModelElement].filter(_.colour.changesAreBanned).toList.sortBy(_.modelElementId.id)

    println("Used symbols =  " + used.size)
    println("Unused size = " + unused.size)
    println("Banned size = " + banned.size)
    println("Used Elements: ")
    used.foreach(e => println(s"  ${e.modelElementId.id} ${e.colour.specific.get}"))
    println("Unused Elements: ")
    unused.foreach(e => println(s"  ${e.modelElementId.id} "))
    println("-------------------------------------------------------------")

  }

  def markRhs(element: ModelElement, purpose: Purpose, path: List[ModelElement], comment: List[String]): Unit = {
    lazy val comment_valDefO = "valDef(outgoing)" :: comment
    lazy val comment_valDef = "valDef" :: comment
    lazy val comment_varDefO = "varDef(outgoing)" :: comment
    lazy val comment_varDef = "varDef" :: comment
    lazy val comment_objO = "obj(outgoing)" :: comment
    lazy val comment_obj = "obj" :: comment


    element.fields.foreach {
      case valDef: ValModel =>
        if (!valDef.isLazy) {
          valDef.refersToElement().foreach { ref =>
            markUsed(ref, markEnclosing = true, purpose, valDef :: path, comment_valDefO)
          }
          markRhs(valDef, purpose, valDef :: path, comment_valDef)
        }
      case varDef: VarModel =>
        varDef.refersToElement().foreach { ref =>
          markUsed(ref, markEnclosing = true, purpose, varDef :: path, comment_varDefO)
        }
        markRhs(varDef, purpose, varDef :: path, comment_varDef)
      //TODO - not sure if this is correct
      // an inner object is lazy in scala, so probably should only be marked when used
      case obj: ObjectModel =>
        obj.refersToElement().foreach { ref =>
          markUsed(ref, markEnclosing = true, purpose, obj :: path, comment_objO)
        }
        markRhs(obj, purpose, obj :: path, comment_obj)
    }
  }
  def markUsed(
      element: ModelElement,
      markEnclosing: Boolean,
      purpose: Purpose,
      path: List[ModelElement],
      comment: List[String]
  ): Unit = {
    val current = element.colour
    if (!current.changesAreBanned && !current.specific.exists(_.hasPurpose(purpose))) {
      if (debug)
        println(s"mark $element as used for $purpose due to ${path.mkString("->")} ${comment.mkString("->")}")

      val newSpecificUsage = current.specific.getOrElse(Usage(purpose)).withPurpose(purpose)
      element.colour = current.withSpecific(newSpecificUsage)
      recordChange(element)
      adjustUsage(element, markEnclosing, purpose, path, comment)
    }
  }
  var trackChanges = false
  val trackedChanges = new mutable.Queue[ModelElement]
  def recordChange(element: ModelElement): Unit = {
    if (trackChanges) element match {
      case _ :ClassLike =>  trackedChanges += element
      case _ =>
    }
  }

  protected def adjustUsage(
      element: ModelElement,
      markEnclosing: Boolean,
      purpose: Purpose,
      path: List[ModelElement],
      comment: List[String]
  ): Unit = {
    val pathWithElement = element :: path

    lazy val comment_internalOutgoingReferences = "internalOutgoingReferences" :: comment
    lazy val comment_rhs = "markRhs" :: comment
    lazy val comment_enclosing = "enclosing" :: comment
    lazy val comment_overrides = "overrides" :: comment
    lazy val comment_overridden = "overridden" :: comment
    lazy val comment_field = "field" :: comment
    lazy val comment_fields = "fields" :: comment

    //all the elements that this refers to
    element.refersToElement().foreach { ref =>
      markUsed(ref, markEnclosing = true, purpose, pathWithElement, comment_internalOutgoingReferences)
    }

    // for the vars, (non lazy) vals and objects - eagerly traverse the RHS, as it is called
    // as the RHS will be executed
    // (its reality) even if we later remove the field
    // we could consider marking at as used differently - a different colour
    //
    // don't mark the fields as used though
    markRhs(element, purpose, pathWithElement, comment_rhs)

    if (markEnclosing) {
      //enclosing
      element.enclosing.foreach { enclosed =>
        markUsed(enclosed, markEnclosing = true, purpose, pathWithElement, comment_enclosing)
      }
    }

    //overrides
    //TODO should probably mark the definition as used only here
    // as the method/class could be abstract, or maybe thats a different rule
    element.overridesElement().foreach { enclosed =>
      markUsed(enclosed, markEnclosing = true, purpose, pathWithElement, comment_overrides)
    }

    //overridden by
    element.overriddenByElement().foreach { enclosed =>
      markUsed(enclosed, markEnclosing = false, purpose.withoutClass, pathWithElement, comment_overridden)
    }
    element match {
      case accessor: AccessorModel =>
        accessor.field.foreach { f =>
          markUsed(f, markEnclosing = true, purpose, pathWithElement, comment_field)
        }
//      case obj: ObjectModel =>
//        //not sure if this is needed. Apply method should be referenced directly
//        //is this a ScalaMeta hangover??
//        obj.methods.foreach { m =>
//          if (m.methodName == "apply")
//            markUsed(m, markEnclosing = false, purpose, pathWithElement, comment apply method of used object")
//        }
      case field: FieldModel =>
        field.declaredIn.foreach { f =>
          markUsed(f, markEnclosing = true, purpose, pathWithElement, comment_fields)
        }
//      //not sure if this is needed.
//      //is this a ScalaMeta hangover??
//      case trt: TraitModel =>
//        trt.fields.foreach { fieldsInTrait =>
//          markUsed(fieldsInTrait, markEnclosing = false, purpose, pathWithElement, comment inside a used trait")
//        }
      case _ =>
    }
  }

  override def runRule(): Unit = {
    runBasicRule()
    runExtraRules()
    runSerialisationRule()
    fixupClassesAndContent()
  }



  def fixupClassesAndContent(): Unit = {
    def processInnards(cls: ClassLike, purpose: Purpose): Unit = {
      assert (purpose.withClass eq purpose)
      val withoutClass = purpose.withoutClass
      cls.allChildren foreach { e =>
        e.colour.specific match {
          case Some (p) if p.hasPurpose(withoutClass) && !p.hasPurpose(purpose) =>
            markUsed(e, false, purpose, e :: Nil, "fixup" :: Nil)
          case _ =>
        }
      }
    }
    def fixClassIfNeeded(ele: ModelElement): Unit = ele match {
      case cls: ClassLike  =>
        cls.colour.specific match {
          case None  => if (cls.colour.changesAreBanned)
            processInnards(cls, Main)
          case Some(p) =>
            if (p.hasPurpose(Main))
              processInnards(cls, Main)
            if (p.hasPurpose(Test))
              processInnards(cls, Test)
        }
      case _ =>
    }

    trackChanges = true
    model.allOf[ClassLike] foreach fixClassIfNeeded

    trackedChanges dropWhile { e => fixClassIfNeeded(e); true}
  }
  def runBasicRule(): Unit = {
    def markEntryUsed(
                  element: ModelElement,
                  markEnclosing: Boolean,
                  purpose: Purpose,
                  start: ModelElement,
                  comment: String
                ): Unit = {
      this.markUsed(element, markEnclosing, purpose, start :: Nil, comment :: Nil)
    }

      allMainEntryPoints.foreach(e => markEntryUsed(e, markEnclosing = true, Main, e, "app"))
    allJunitTest.foreach(e => markEntryUsed(e, markEnclosing = true, Test, e, "junit test"))
    allJunitClasses.foreach { testClass =>
      markEntryUsed(testClass, markEnclosing = true, Test, testClass, "junit test class")
    }
    allScalaTests.foreach(e => markEntryUsed(e, markEnclosing = true, Test, e, "scalatests"))

    (model.allOf[MethodModel] ++ model.allOf[FieldModel]).filter(_.overridesExternal) foreach {e =>
      markEntryUsed(e, markEnclosing = false, MainNotClass, e, "external API implementation/override")
    }

    //we dont really want to do this but we don't successfully remove parameters,
    // and classes parameters overlap with the val
    model.allOf[FieldModel].filter(_.isParameter) foreach { e=>
      e.mark = Mark.dontChange(SimpleReason("parameter") )
    }
    model.allOf[FieldModel].filter(_.associatedConstructorParameter.isDefined) foreach { e=>
      e.mark = Mark.dontChange(SimpleReason("has ctor val") )
      e.associatedConstructorParameter.get.enclosing.foreach {
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

    def markAllUsed(e: ModelElement, comment: String): Unit = {
      markEntryUsed(e, false, Main, e, comment)
      e.allChildren foreach (markAllUsed(_, comment))
    }
    if (options.externalInterface.nonEmpty || options.generatedSource.nonEmpty) {
      for (source <- model.allOf[SourceModel]) {
        val md = sourceMetaData(source)
        if (md.external) markAllUsed(source, "external API")
        else if (md.generated) markAllUsed(source, "generated API")
      }
      if (options.externalInterface.nonEmpty) {
        val externalElements = model.allOf[ModelElement].toStream.par.filter { e =>
          val id = e.modelElementId.id
          options.externalInterface.exists(_.pattern.matcher(id).matches())
        } toList

        externalElements.foreach(e => markEntryUsed(e, false, Main, e, "external API"))
      }
    }

  }
  def runExtraRules(): Unit = {}
  def runSerialisationRule(): Unit = {
    val serialisationCode = "serialisationCode" :: Nil
    allSerialisationEntries.foreach(e =>
    //we don't really want to mark a class as used just because it had a serialisation method
    //so we limit it to just those that seem to be in use already
    //you could also consider that all serialisable classes are live ....
      if (!e.classOrEnclosing.colour.isInitial)
        markUsed(e, markEnclosing = false, Main.withoutClass, e :: Nil, serialisationCode))
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

          case cls: ClassLike =>
            if (debug)
              log(" cls element handling")
            if (element.colour.isInitial && element.existsInSource) {
              remove(element, s"Simple ${element.name} (${element.getClass} unused")
              false
            } else if (element.colour.specific.isDefined && !element.colour.specific.get.hashSomeMainPurpose && element.existsInSource) {
              remove(element, s"Simple ${element.name} (${element.getClass} unused")
              false
            } else
              true
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
    private val data = Array.tabulate(64)(new Usage(_))
    def apply (p: Purpose) = {
      require (p.id != 0)
      data(p.id)
    }
  }
  case class Usage private (existingPurposes: Int) extends SomeSpecificColour {
    def hashSomeMainPurpose: Boolean = hasPurpose(Main) || hasPurpose(Test)

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

    override def toString: String = {
      var res = ""
      if (hasPurpose(Main))
        res += "Main, "
      if (hasPurpose(MainNotClass))
        res += "MainNotClass, "
      if (hasPurpose(Test))
        res += "Test, "
      if (hasPurpose(TestNotClass))
        res += "TestNotClass, "

      if (!res.isEmpty) res = res.substring(0, res.length - 2)
      s"[$res]"
    }
  }

  sealed trait WithClass {
    self: Purpose =>
    override final def withClass = self
  }
  sealed trait WithoutClass {
    self: Purpose =>
    override final def withoutClass = self
  }
  object Main extends Purpose with WithClass {
    override def id: Int = 1
    override def withoutClass: Purpose = MainNotClass
  }
  object MainNotClass extends Purpose  with WithoutClass{
    override def id: Int = 1 << 1
    override def withClass: Purpose = Main
  }

  object Test extends Purpose with WithClass {
    override def id: Int = 1 << 2
    override def withoutClass: Purpose = TestNotClass
  }
  object TestNotClass extends Purpose with WithoutClass{
    override def id: Int = 1 << 3
    override def withClass: Purpose = Test
  }
  val mainUsage = Usage(Main)
  val testUsage = Usage(Test)

}
class AbstractDeadCodeCommandLine extends ScalaCleanCommandLine
