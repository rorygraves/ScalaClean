package scalaclean.rules.privatiser

import org.scalaclean.analysis.plugin.VisibilityData
import scalaclean.cli.RunOptions
import scalaclean.model._
import scalaclean.rules.AbstractRule
import scalaclean.util.ScalaCleanTreePatcher
import scalafix.v1.SyntacticDocument

import scala.meta.io.AbsolutePath
import scala.meta.tokens.Token

class Privatiser(model: ProjectModel, options: RunOptions) extends AbstractRule("Privatiser", model, options) {

  type Colour = PrivatiserLevel

  override def markInitial(): Unit = {
    model.allOf[ModelElement].foreach(e => e.colour = Undefined)
  }

  override def runRule(): Unit = {
    model.allOf[ModelElement].foreach {
      case e: SourceModel =>
        e.colour = NoChange("source")
      case e => e.colour = localLevel(e)
    }
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
    if (element.colour == Undefined) {
      val colour = element match {
        case x if x.modelElementId.isLocal                                   => NoChange("its local")
        case x if !x.existsInSource                                          => NoChange("no source")
        case s: SourceModel                                                  => NoChange("source")
        case x if inMethod(x)                                                => NoChange("in a method and not visible")
        case fieldsModel: FieldsModel                                        => calcFieldsLevel(fieldsModel)
        case fieldModel: FieldModel if fieldModel.inCompoundFieldDeclaration => localLevel(fieldModel.declaredIn.get)
        case getterMethodModel: GetterMethodModel                            => localLevel(getterMethodModel.field.get)
        case setterMethodModel: SetterMethodModel                            => localLevel(setterMethodModel.field.get)
        case fieldModel: FieldModel                                          => calcFieldLevel(fieldModel)
        case _ =>
          calcSingleLevel(element)
      }
      element.colour = colour
    }
    element.colour
  }

  def calcFieldLevel(field: FieldModel): PrivatiserLevel = {
    field.accessors.foldLeft(calcSingleLevel(field)) { case (level, accessor) =>
      level.widen(calcSingleLevel(accessor))
    }
  }

  def calcFieldsLevel(fieldsModel: FieldsModel): PrivatiserLevel = {
    fieldsModel.fieldsInDeclaration.foldLeft(Undefined: PrivatiserLevel) { case (level, field) =>
      level.widen(calcFieldLevel(field))
    }
  }

  def calcSingleLevel(element: ModelElement): PrivatiserLevel = {
    val incoming = {
      element.internalIncomingReferences.map(_._1)
    }.toSet - element -- (element match {
      case f: FieldModel    => f.accessors
      case a: AccessorModel => a.field
      case _                => Nil
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

    incoming.foreach { ref: ModelElement =>
      val isFromChild = ref.classOrEnclosing.xtends(enclosing.modelElementId)
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
    res = res.widen(Scoped.Private(ElementId.childThis(enclosing.modelElementId), s"at least self"))
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

    object visitor extends ScalaCleanTreePatcher(patchStats, syntacticDocument) {

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

      override def debug: Boolean       = options.debug
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
              modelElement.colour.asText(modelElement).foreach(v => changeVisibility(v))
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
    visitor.visit(sModel)

    val result = visitor.result
    if (debug) {
      println("--------NEW----------")
      result.foreach(println)
      println("------------------")
    }
    result
  }

}
