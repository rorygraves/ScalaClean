package scalaclean.rules.privatiser

import org.scalaclean.analysis.plugin.VisibilityData
import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.RuleRun
import scalaclean.util.ScalaCleanTreePatcher
import scalafix.v1.SyntacticDocument

import scala.meta.io.AbsolutePath
import scala.meta.tokens.Token

abstract class AbstractPrivatiser[T <: AbstractPrivatiserCommandLine](val options: T, val model: ProjectModel)
    extends RuleRun[T] {

  type Colour = PrivatiserLevel

  override def markInitial(): Unit = {
    model.allOf[ModelElement].foreach(e => e.colour = Undefined)
  }

  def markKnown(): Unit = {
    allApp.foreach(e => e.mark = NoChange("It's an app"))

    allTestEntryPoints.foreach(e => e.mark = NoChange("It's a test"))

    model.allOf[ModelElement].foreach {
      case x if x.colour != Undefined => //already covered

      case e if isPublic(e) =>
        e.colour = NoChange("rule/annotation")
      case e if e.modelElementId.isLocal =>
        e.colour = NoChange("its local")
      case e if !e.existsInSource =>
        e.colour = NoChange("no source")
      case e: SourceModel =>
        e.colour = NoChange("source")
      case e if inMethod(e) =>
        e.colour = NoChange("in a method and not visible")
      case fieldModel: FieldModel if fieldModel.inCompoundFieldDeclaration =>
        fieldModel.colour = NoChange("in a compound field declaration")

      case _ => //do nothing
    }
  }

  def isPublic(e: ModelElement): Boolean = {
    // e.symbol.symbol.value.endsWith("_:=) ||
    e.annotations.exists(ad => otherAnnotationBasedEntryPoints.contains(ad.fqName))
  }

  def ruleSpecific(): Unit = {
    model.allOf[ModelElement].foreach { case e => e.colour = localLevel(e) }
  }

  override def runRule(): Unit = {
    markKnown()
    ruleSpecific()
    if (debug)
      model.allOf[ModelElement].toList.sortBy(_.infoPosSorted).foreach(ele => println(s"$ele  colour: ${ele.colour}"))
  }

  def inMethod(element: ModelElement): Boolean = {
    def isOrInMethod(element: ModelElement): Boolean = {
      element.isInstanceOf[MethodModel] ||
      element.enclosing.exists(isOrInMethod)
    }

    element.enclosing.exists(isOrInMethod)
  }

  def localLevel(element: ModelElement): PrivatiserLevel = {
    element match {
      case _ if element.colour != Undefined => element.colour
      case _ if element.enclosing.size == 1 =>
        element.enclosing.head match {
          case sourceFile: SourceModel if element.isInstanceOf[ClassLike] =>
            determineIfPrivate(element, element.modelElementId)

          case classLike: ClassLike =>
            determineIfPrivate(element, classLike.modelElementId)

          case _ =>
            NoChange(s"Its not in a classLike so private already")
        }
    }
  }

  def determineIfPrivate(element: ModelElement, myClassLike: ElementId): PrivatiserLevel = {
    element match {
      case fieldModel: FieldModel =>
        determineAccess(element, myClassLike, incomingReferences(fieldModel))
      case fieldsModel: FieldsModel =>
        determineAccess(element, myClassLike, fieldsModel.fieldsInDeclaration.flatMap(incomingReferences))
      case e =>
        determineAccess(element, myClassLike, e.incomingReferences)
    }
  }

  def incomingReferences(fieldModel: FieldModel) = {
    //all of the direct references + all of the references in from the getters/setters that are compiler generated ( not in source)
    //and filter out the internal accesses from the accessors
    val generatedAccessors = fieldModel.accessors.filter(!_.existsInSource).toList
    (fieldModel.incomingReferences.filterNot { case refers: Refers =>
      generatedAccessors.contains(refers.fromElement)
    }) ++ generatedAccessors.flatMap(_.incomingReferences)
  }

  def determineAccess(
      element: ModelElement,
      myClassLike: ElementId,
      incomingReferences: Iterable[Refers]
  ): PrivatiserLevel = {

    var res: PrivatiserLevel = Undefined

    incomingReferences.foreach { case refers: Refers =>
      val ref         = refers.fromElement
      val isFromChild = ref.classOrEnclosing.xtends(myClassLike)
      val access =
        if (isFromChild)
          Scoped.Protected(ref.modelElementId, s"accessed from $ref", forceProtected = false)
        else
          Scoped.Private(ref.modelElementId, s"accessed from $ref")
      res = res.widen(access)
    }
    //we must be visible to anything that overrides us
    element.internalDirectOverriddenBy.foreach { overriddenBy =>
      res = res.widen(
        Scoped.Protected(overriddenBy.modelElementId, s"overridden in from $overriddenBy", forceProtected = false)
      )
    }

    //We must be at least as visible as anything that we override
    element.allDirectOverrides.foreach {
      case (Some(overriddenModel), _) =>
        val overriddenVisibility = localLevel(overriddenModel) match {
          case s: Scoped if s.isProtected => s.copy(forceProtected = true)
          case other                      => other
        }
        res = res.widen(overriddenVisibility)
      case (None, _) =>
        //if it is not in the model, then we will leave this as is
        res = res.widen(NoChange("inherits from external"))
    }
    res = res.widen(Scoped.Private(ElementId.childThis(myClassLike), s"at least self"))
    res
  }

  var elementsObserved = 0
  var elementsChanged  = 0

  override def fix(targetFile: AbsolutePath, syntacticDocument: () => SyntacticDocument): List[SCPatch] = {
    val targetFileName = targetFile.toString
    // find source model
    val sModel = model
      .allOf[SourceModel]
      .filter(_.toString.contains(targetFileName))
      .toList
      .headOption
      .getOrElse(throw new IllegalStateException(s"Unable to find source model for $targetFileName"))

    val visitor: PrivatiserVisitor = newPrivatiserVisitor(syntacticDocument)
    visitor.visit(sModel)

    val result = visitor.result
    if (debug) {
      println("--------NEW----------")
      result.foreach(println)
      println("------------------")
    }
    result
  }

  def newPrivatiserVisitor(syntacticDocument: () => SyntacticDocument) = new PrivatiserVisitor(syntacticDocument)

  class PrivatiserVisitor(syntacticDocument: () => SyntacticDocument)
      extends ScalaCleanTreePatcher(patchStats, syntacticDocument) {

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

    override def debug: Boolean = options.debug

    override def addComments: Boolean = options.addComments

    override protected def visitInSource(modelElement: ModelElement): Boolean = {
      if (!modelElement.modelElementId.isLocal) {

        def changeVisibility(visText: String) = {
          val currentVis = modelElement.extensionOfType[VisibilityData].getOrElse(VisibilityData.PUBLIC)

          val expectedTokens: Seq[String] = {
            currentVis match {
              case VisibilityData(start, end, "", None)         => List()
              case VisibilityData(start, end, vis, None)        => List(vis)
              case VisibilityData(start, end, vis, Some(scope)) => List(vis, "[", scope.innerScopeString, "]")
            }
          }
          val beginIndex = tokens.indexWhere(t => t.start == modelElement.rawStart)
          assert(beginIndex != -1)

          def find(
              startIndex: Int,
              text: String,
              maxIndex: Option[Int] = None,
              maxPos: Option[Int] = None
          ): (Int, Token) = {
            def showTokens(start: Int, count: Int, search: String, msg: String): Unit = {
              (start to start + count).foreach(i => println(s" token #$i ${tokens(i)}"))
              throw new IllegalStateException(s"$msg")
            }

            val found = tokens.indexWhere(t => t.text == text, startIndex)
            assert(found != -1, showTokens(startIndex, 10, text, s"Can't find '$text'"))
            maxIndex.foreach { i =>
              assert(found <= i, showTokens(startIndex, 10, text, s"too far when looking for '$text''"))
            }
            maxPos.foreach { p =>
              assert(tokens(found).start <= p, showTokens(startIndex, 10, text, s"too far when looking for '$text''"))
            }

            (found, tokens(found))
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
          case fieldModel: FieldModel if fieldModel.declaredIn.nonEmpty      =>
          case accessorModel: AccessorModel if accessorModel.field.isDefined =>
          case _ =>
            modelElement.colour.asText(modelElement, options).foreach(v => changeVisibility(v))
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
