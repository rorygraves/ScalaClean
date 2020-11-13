package scalaclean.rules.finaliser

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.{AbstractRule, RuleRun, SourceFile}
import scalaclean.util.{ScalaCleanTreePatcher, SingleFileVisit}

import scala.annotation.tailrec
import scala.reflect.internal.Flags

//marks things final and sealed were it can

object Finaliser extends AbstractRule[FinaliserCommandLine] {
  override type Rule = Finaliser

  override def cmdLine                                                   = new FinaliserCommandLine
  override def apply(options: FinaliserCommandLine, model: AllProjectsModel) = new Rule(options, model)
}

class Finaliser(override val options: FinaliserCommandLine, override val model: AllProjectsModel)
    extends RuleRun[FinaliserCommandLine] {

  type SpecificColour = FinaliserLevel

  object dontChangeBecause {
    val sourceFile            = dontChange("source file")
    val local                 = dontChange("its local")
    val noSource              = dontChange("no source")
    val inMethod              = dontChange("in a method and not visible")
    val isObject              = dontChange("object is effectively final")
    val isVar                 = dontChange("var is always final")
    val isFinal               = dontChange("its already final")
    val isPrivate             = dontChange("its private so probably not worth the change")
    val compoundField         = dontChange("part of a compound decl")
    val ownerIsFinalClass     = dontChange("owner is a final class")
    val ownerIsObject         = dontChange("owner is a an object")
    val ownerIsPrivateOrFinal = dontChange("owner is private/final")
    val ownerWillBeFinal      = dontChange("owner will be a final class")

    val itsGenerated = dontChange("its a generated API")
    val itsExternal = dontChange("its an external API")

  }

  object changeTo {
    val keepOpen   = makeChange(Open(""))
    val makeSealed = makeChange(Sealed(""))
    val makeFinal  = makeChange(Final)
  }

  override def runRule(): Unit = {

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

    model.allOf[ModelElement].foreach {
      case e: SourceModel =>
        e.colour = dontChangeBecause.sourceFile
      case e => e.colour = localLevel(e)
    }
    if (options.debug)
      model.allOf[ModelElement].toList.sortBy(_.infoPosSorted).foreach(ele => println(s"$ele  colour: ${ele.colour}"))
  }

  def inMethod(element: ModelElement): Boolean = {
    def isOrInMethod(element: ModelElement): Boolean = {
      element.isInstanceOf[MethodModel] ||
      element.enclosing.exists(isOrInMethod)
    }

    element.enclosing.exists(isOrInMethod)
  }

  def localLevel(element: ModelElement): Colour = {
    if (element.colour.isInitial) {
      val newMark: Colour = element match {
        case x if x.modelElementId.isLocal => dontChangeBecause.local
        case x if !x.existsInSource        => dontChangeBecause.noSource
        case _: SourceModel                => dontChangeBecause.sourceFile
        case x if inMethod(x)              => dontChangeBecause.inMethod
        case _: ObjectModel                => dontChangeBecause.isObject
        case _: VarModel                   => dontChangeBecause.isVar
        case _ if (element.isFinal)        => dontChangeBecause.isFinal
        case _
            if element.isPrivate
            //fields are marked private, so exclude them and handle in calcFieldLevel
              && !element.isInstanceOf[FieldModel] =>
          dontChangeBecause.isPrivate
        case fieldsModel: FieldsModel if (fieldsModel.fields.exists(_.isInstanceOf[VarModel])) =>
          dontChangeBecause.isVar
        case fieldModel: FieldModel if fieldModel.inCompoundFieldDeclaration =>
          dontChangeBecause.compoundField

        case fieldsModel: FieldsModel => calcFieldsLevel(fieldsModel)
        case methodModel: MethodModel => calcMethodLevel(methodModel)
        case fieldModel: FieldModel   => calcFieldLevel(fieldModel)
        case clsOrTrait: ClassLike    => calcClassLevel(clsOrTrait)
      }
      element.mark = newMark
    }
    element.colour
  }

  def calcMethodLevel(method: MethodModel): Colour = {
    method.enclosing.head match {
      case cls: ClassModel if localLevel(cls).specific.contains(Final) => dontChangeBecause.ownerIsFinalClass
      case cls: ObjectModel                            => dontChangeBecause.ownerIsObject
      case cls: ClassLike =>
        if (method.overriddenByElement().isEmpty) changeTo.makeFinal
        else changeTo.keepOpen
      case encl => dontChange(s"enclosed in $encl")
    }

  }

  def calcFieldLevel(field: FieldModel): Colour = {
    field.declaredIn.getOrElse(field).enclosing.head match {
      case cls: ClassModel if cls.isFinal || cls.isPrivate => dontChangeBecause.ownerIsPrivateOrFinal
      case cls: ClassModel if localLevel(cls).specific.contains(Final)     => dontChangeBecause.ownerWillBeFinal
      case cls: ObjectModel                                => dontChangeBecause.ownerIsObject
      case cls: ClassLike                                  =>
        // we need to consider the accessors. The val isn't overridden, but if the accessor is then we cant make it final
        // in the parent
        val overrides = field.overriddenByElement() ++ field.accessors.flatMap(_.overriddenByElement())
        if (overrides.isEmpty)
          changeTo.makeFinal
        else {
          val first = overrides.next
          val size = overrides.size + 1
          dontChange(s"$size overrides - e.g. ${first}")
        }
      case encl => dontChange(s"enclosed in $encl")
    }
  }

  def calcFieldsLevel(fieldsModel: FieldsModel): Colour = {
    fieldsModel.fieldsInDeclaration.foldLeft(calcFieldLevel(fieldsModel.fieldsInDeclaration.head)) {
      case (colour, field) =>
        colour.merge(calcFieldLevel(field))
    }
  }

  @tailrec final def getSource(element: ModelElement): SourceModel = element match {
    case sourceModel: SourceModel => sourceModel
    case _                        => getSource(element.enclosing.head)
  }

  def calcClassLevel(classLike: ClassLike): Colour = classLike match {
    case model: ClassModel =>
      val ext = model.extendedByElement()
      if (ext.isEmpty)
        changeTo.makeFinal
      else {
        val mySource = getSource(model)
        if (ext.forall(cls => getSource(cls) == mySource))
          changeTo.makeSealed
        else
          changeTo.keepOpen
      }
    case model: ObjectModel => dontChangeBecause.isObject
    case model: TraitModel =>
      val mySource = getSource(model)
      val ext      = model.extendedByElement()
      if (ext.forall(cls => getSource(cls) == mySource))
        changeTo.makeSealed
      else
        changeTo.keepOpen

  }

  override def generateFixes(sourceFile: SourceFile): SingleFileVisit = {

    object visitor extends ScalaCleanTreePatcher(sourceFile, Finaliser.this.options) {

      def handleDecl(modelElement: ModelElement): Unit = {
        modelElement.colour.specific match {
          case None                 =>
          case Some(Open(reason))   =>
          case Some(Sealed(reason)) =>
          case Some(Final) =>
            if ((Flags.FINAL & modelElement.flags) == 0)
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "final ", ""))
        }
      }
      def handleClass(modelElement: ModelElement): Unit = {
        modelElement.colour.specific match {
          case Some(Sealed(reason)) =>
            val isSealed = (Flags.SEALED & modelElement.flags) != 0
            if (!isSealed)
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "sealed ", ""))
          case Some(Final) =>
            val isFinal  = (Flags.FINAL & modelElement.flags) != 0
            val isSealed = (Flags.SEALED & modelElement.flags) != 0
            //TODO cope with sealed -> final
            if (!isFinal && !isSealed)
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "final ", ""))
          case Some(Open(reason)) =>
          case None               =>
        }
      }

      override protected def visitInSource(modelElement: ModelElement): Boolean = {
        modelElement match {
          case fieldModel: FieldModel if fieldModel.declaredIn.nonEmpty      =>
          case accessorModel: AccessorModel if accessorModel.field.isDefined =>
          case _: MethodModel | _: FieldModel | _: FieldsModel               =>
//            elementsObserved += 1
            handleDecl(modelElement)
          case classLike: ClassLike =>
//            elementsObserved += 1
            handleClass(modelElement)
        }
        true
      }
    }
    visitor.visit(sourceFile.file)

    visitor.result
  }

}

class FinaliserCommandLine extends ScalaCleanCommandLine
