package scalaclean.rules.privatiser

import org.scalaclean.analysis.plugin.VisibilityData
import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.{RuleRun, SourceFile}
import scalaclean.util.{ScalaCleanTreePatcher, SingleFileVisit}
import scala.meta.tokens.Token

abstract class AbstractPrivatiser[T <: AbstractPrivatiserCommandLine](val options: T, val model: AllProjectsModel)
    extends RuleRun[T] {

  type SpecificColour = PrivatiserLevel

  object dontChangeBecause {
    val itsAnApp             = dontChange("It's an app")
    val itsATest             = dontChange("It's a test")
    val itsARuleOrAnnotation = dontChange("rule/annotation")
    val itsLocal             = dontChange("its local")
    val itsNotInSource       = dontChange("no source")
    val itsASourceFile       = dontChange("source file")
    val itsInAMethod         = dontChange("in a method and not visible")
    val itsInACompoundField  = dontChange("in a compound field declaration")
    val notInClassLike       = dontChange(s"Its not in a classLike so private already")
    val inheritsFromExternal = dontChange("inherits from external")

    val itsAParameter = dontChange("its a parameter")
    val itaADefaultAccessor = dontChange("its a default accessor")
    val itsGenerated = dontChange("its a generated API")
    val itsExternal = dontChange("its an external API")
  }

  def markKnown(): Unit = {
    allApp.foreach(e => e.mark = dontChangeBecause.itsAnApp)

    allTestEntryPoints.foreach(e => e.mark = dontChangeBecause.itsATest)

    model.allOf[ModelElement].foreach {
      case x if !x.colour.isInitial => //already covered

      case e if isPublic(e) =>
        e.colour = dontChangeBecause.itsARuleOrAnnotation
      case e if e.modelElementId.isLocal =>
        e.colour = dontChangeBecause.itsLocal
      case e if !e.existsInSource =>
        e.colour = dontChangeBecause.itsNotInSource
      case e: SourceModel =>
        e.colour = dontChangeBecause.itsASourceFile
      case e if inMethod(e) =>
        e.colour = dontChangeBecause.itsInAMethod
      case fieldModel: FieldModel if fieldModel.inCompoundFieldDeclaration =>
        fieldModel.colour = dontChangeBecause.itsInACompoundField

      case _ => //do nothing
    }
    //we dont really want to do this but we don't successfully handle parameters,
    // and classes parameters overlap with the val
    model.allOf[FieldModel].filter(_.isParameter) foreach { e=>
      e.mark = dontChangeBecause.itsAParameter
    }
    // defaul accessors are internal to the compiler model
    model.allOf[PlainMethodModel].filter(_.defaultAccessorFor.isDefined) foreach { e=>
      e.mark = dontChangeBecause.itaADefaultAccessor
    }

    def markAllUsed(e: ModelElement, reason: Mark[SpecificColour]): Unit = {
      e.mark = reason
      e.allChildren foreach (markAllUsed(_, reason))
    }
    if (options.externalInterface.nonEmpty || options.generatedSource.nonEmpty) {
      for (source <- model.allOf[SourceModel]) {
        val md = sourceMetaData(source)
        if (md.external) markAllUsed(source, dontChangeBecause.itsExternal)
        else if (md.generated) markAllUsed(source, dontChangeBecause.itsGenerated)
      }
      if (options.externalInterface.nonEmpty) {
        val externalElements = model.allOf[ModelElement].toStream.par.filter { e =>
          val id = e.modelElementId.id
          options.externalInterface.exists(_.pattern.matcher(id).matches())
        } toList

        externalElements.foreach(_.mark = dontChangeBecause.itsExternal)
      }
    }
  }

  def isPublic(e: ModelElement): Boolean = {
    // e.symbol.symbol.value.endsWith("_:=) ||
    e.annotations.exists(ad => otherAnnotationBasedEntryPoints.contains(ad.fqName))
  }

  def ruleSpecific(): Unit = {
    model.allOf[ModelElement].foreach(e => e.colour = localLevel(e))
  }

  override def runRule(): Unit = {
    markKnown()
    ruleSpecific()
    for (cls <- model.allOf[ClassLike];
         self <- cls.selfType) {
      self.mark = Mark.dontChange(SimpleReason("self type field") )
    }

    if (debug)
      model.allOf[ModelElement].toList.sortBy(_.infoPosSorted).foreach(ele => println(s"$ele  colour: ${ele.colour}"))
  }

  def inMethod(element: ModelElement): Boolean = {
    element.enclosingOf[MethodModel].isDefined
  }

  def localLevel(element: ModelElement): Colour = {
    if (!element.colour.isInitial) element.colour
    else element.enclosing match {
      case Some(sourceFile: SourceModel) if element.isInstanceOf[ClassLike] =>
        determineIfPrivate(element, element.modelElementId)
      case Some(classLike: ClassLike) =>
        determineIfPrivate(element, classLike.modelElementId)
      case Some(_) =>
        dontChangeBecause.notInClassLike
      case None =>
        dontChangeBecause.notInClassLike
    }
  }

  def determineIfPrivate(element: ModelElement, myClassLike: ElementId): Colour = {
    element match {
      case fieldModel: FieldModel =>
        determineAccess(element, myClassLike, incomingReferences(fieldModel))
      case fieldsModel: FieldsModel =>
        determineAccess(element, myClassLike, fieldsModel.fieldsInDeclaration.iterator.flatMap(incomingReferences))
      case e =>
        determineAccess(element, myClassLike, e.referredToByElement())
    }
  }

  def incomingReferences(fieldModel: FieldModel): Iterator[ModelElement] = {
    //all of the direct references + all of the references in from the getters/setters that are compiler generated ( not in source)
    //and filter out the internal accesses from the accessors
    val generatedAccessors = fieldModel.accessors.filter(!_.existsInSource).toSet[ModelElement]
    (fieldModel.referredToByElement().filterNot { e =>
      generatedAccessors.contains(e)
    }) ++ generatedAccessors.flatMap(_.referredToByElement())
  }

  def determineAccess(
      element: ModelElement,
      myClassLike: ElementId,
      incomingReferences: Iterator[ModelElement]
  ): Colour = {

    var res: Colour = Mark.initial[PrivatiserLevel]

    incomingReferences.foreach { ref =>
      val isFromChild = ref.classOrEnclosing.xtends(myClassLike)
      val access: Colour =
        if (isFromChild)
          makeChange(Scoped.Protected(ref.modelElementId, s"accessed from $ref", forceProtected = false))
        else
          makeChange(Scoped.Private(ref.modelElementId, s"accessed from $ref"))
      res = res.merge(access)
    }
    //we must be visible to anything that overrides us
    element.overriddenByElement(direct=Some(true)).foreach { overriddenBy =>
      res = res.merge(
        makeChange(
          Scoped.Protected(overriddenBy.modelElementId, s"overridden in from $overriddenBy", forceProtected = false)
        )
      )
    }

    //We must be at least as visible as anything that we override
    element.overridesFull(direct=Some(true)).map(_.elementIfDefined).foreach {
      case Some(overriddenModel) =>
        val overriddenVisibility = localLevel(overriddenModel).specific match {
          case Some(s: Scoped) if s.isProtected => makeChange(s.copy(forceProtected = true))
          case other                            => localLevel(overriddenModel)
        }
        res = res.merge(overriddenVisibility)
      case None =>
        //if it is not in the model, then we will leave this as is
        res = dontChangeBecause.inheritsFromExternal
    }
    res = res.merge(makeChange(Scoped.Private(ElementIds.childThis(myClassLike), s"at least self")))
    res
  }

  override def generateFixes(sourceFile: SourceFile): SingleFileVisit = {

    object visitor extends ScalaCleanTreePatcher(sourceFile, AbstractPrivatiser.this.options) {

      // TODO CURRENT UNUSED
      //      def existingAccess(mods: Seq[Mod]): (Option[Mod], PrivatiserLevel) = {
      //        val res: Option[(Option[Mod], PrivatiserLevel)] = mods.collectFirst {
      //          case s@Mod.Private(scope) => (Some(s), Scoped.Private(OldElementId.fromTree(scope), "existing"))
      //          case s@Mod.Protected(scope) => (Some(s), Scoped.Protected(OldElementId.fromTree(scope), "existing", forceProtected = false))
      //        }
      //        res.getOrElse((None, Public("existing")))
      //      }
      //
      //      private def changeAccessModifier(
      //                                        level: PrivatiserLevel, mods: Seq[Mod], defn: Stat, aModel: ModelElement, forcePosition: Option[Token]): Boolean = {
      //        val (mod, existing) = existingAccess(mods)
      //        val proposed = level.asText(aModel)
      //
      //        def buildInsertion(toReplace: String): Boolean = {
      //          forcePosition match {
      //            case Some(token) =>
      //              lb.append(SCPatch(token.start, token.start, s"$toReplace "))
      //              true
      //            case None =>
      //              val tokens = defn.tokens
      //              tokens.find {
      //                _.start == aModel.rawStart
      //              } match {
      //                case Some(token) =>
      //                  lb.append(SCPatch(token.start, token.start, s"$toReplace "))
      //                  true
      //                case None =>
      //                  //probably quite worrying
      //                  lb.append(SCPatch(defn.pos.start, defn.pos.start, s"$toReplace "))
      //                  true
      //              }
      //          }
      //        }
      //
      //        val structuredPatch: Boolean =
      //          (mod, level.shouldReplace(aModel), proposed) match {
      //            case (_, _, None) => false
      //            case (None, _, Some(toReplace)) =>
      //              buildInsertion(toReplace)
      //            case (_, false, Some(toReplace)) =>
      //              buildInsertion(toReplace)
      //            case (Some(existing), true, Some(toReplace)) =>
      //              lb.append(SCPatch(existing.pos.start, existing.pos.end, s"$toReplace"))
      //              true
      //          }
      //        val updatedMarker = level.marker2(defn).map { v => lb.append(v); true }.getOrElse(false)
      //
      //        structuredPatch || updatedMarker
      //
      //      }
      //

      override protected def visitInSource(modelElement: ModelElement): Boolean = {
        if (!modelElement.modelElementId.isLocal) {

          def changeVisibility(visText: String) = {
            val currentVis = modelElement.extensionOfType[VisibilityData].getOrElse(VisibilityData.PUBLIC)

            val expectedTokens: Seq[String] = {
              currentVis match {
                case VisibilityData(start, end, "", None) => List()
                case VisibilityData(start, end, vis, None) => List(vis)
                case VisibilityData(start, end, vis, Some(scope)) => List(vis, "[", scope.innerScopeString, "]")
              }
            }
            val beginIndex = tokenArray.indexWhere(t => t.start == modelElement.rawStart)
            assert(beginIndex != -1)

            def find(
                      startIndex: Int,
                      text: String,
                      maxIndex: Option[Int] = None,
                      maxPos: Option[Int] = None
                    ): (Int, Token) = {
              def showTokens(start: Int, count: Int, search: String, msg: String): Unit = {
                (start to start + count).foreach(i => println(s" token #$i ${tokenArray(i)}"))
                throw new IllegalStateException(s"$msg")
              }

              val found = tokenArray.indexWhere(t => t.text == text, startIndex)
              assert(found != -1, showTokens(startIndex, 10, text, s"Can't find '$text'"))
              maxIndex.foreach { i =>
                assert(found <= i, showTokens(startIndex, 10, text, s"too far when looking for '$text''"))
              }
              maxPos.foreach { p =>
                assert(tokenArray(found).start <= p, showTokens(startIndex, 10, text, s"too far when looking for '$text''"))
              }

              (found, tokenArray(found))
            }

            val (targetStart, targetEnd) =
              if (expectedTokens.isEmpty) (modelElement.rawStart, modelElement.rawStart)
              else {
                val start = find(beginIndex, expectedTokens.head, maxPos = Some(modelElement.rawFocusStart))
                val end = expectedTokens.tail.foldLeft(start) { case (index, text) =>
                  find(index._1, text, maxPos = Some(modelElement.rawFocusStart))
                }
                (start._2.start, end._2.end)
              }
            if (debug)
              log(s" TargetPos = $targetStart -> $targetEnd")

            val replacementText = if (targetStart == targetEnd) visText + " " else visText
            this.collect(SCPatch(targetStart, targetEnd, replacementText))
          }

          modelElement match {
            case fieldModel: FieldModel if fieldModel.declaredIn.nonEmpty =>
            case accessorModel: AccessorModel if accessorModel.field.isDefined =>
            case _ =>
              modelElement.colour.specific.foreach(_.asText(modelElement, options).foreach(v => changeVisibility(v)))
          }
        }

        // should be traverse deeper
        !modelElement.modelElementId.isLocal && (modelElement match {
          case _: ClassLike =>
            true
          case _: MethodModel | _: FieldModel | _: FieldsModel =>
            false
          case _ =>
            true
        })
      }
    }

      visitor.visit(sourceFile.file)

      visitor.result
  }

}

import org.kohsuke.args4j.{ Option => ArgOption }

abstract class AbstractPrivatiserCommandLine extends ScalaCleanCommandLine {

  @ArgOption(
    name = "--reduceDuplicateScopeChanges",
    usage = "reduces for example the number of private methods in private classes",
    required = true
  )
  var reduceDuplicateScopeChanges: Boolean = false

}
