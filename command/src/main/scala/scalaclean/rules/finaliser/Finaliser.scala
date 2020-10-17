package scalaclean.rules.finaliser

import scalaclean.cli.{RunOptions, ScalaCleanCommandLine}
import scalaclean.model._
import scalaclean.rules.{AbstractRule, RuleRun}
import scalaclean.util.ScalaCleanTreePatcher
import scalafix.v1.SyntacticDocument

import scala.annotation.tailrec
import scala.meta.io.AbsolutePath
import scala.reflect.internal.Flags

//marks things final and sealed were it can

object Finaliser extends AbstractRule[FinaliserCommandLine] {
  override type Rule    = Finaliser

  override def cmdLine = new FinaliserCommandLine
  override def apply(options: FinaliserCommandLine, model: ProjectModel) = new Rule(options, model)
}

class Finaliser(override val options: FinaliserCommandLine, override val model: ProjectModel) extends RuleRun[FinaliserCommandLine] {

  type Colour = FinaliserLevel

  override def markInitial(): Unit = {
    model.allOf[ModelElement].foreach(e => e.colour = Undefined)
  }

  override def runRule(): Unit = {
    model.allOf[ModelElement].foreach {
      case e: SourceModel =>
        e.colour = NoChange("source")
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
    if (element.colour == Undefined) {
      val colour = element match {
        case x if x.modelElementId.isLocal => NoChange("its local")
        case x if !x.existsInSource        => NoChange("no source")
        case _: SourceModel                => NoChange("source")
        case x if inMethod(x)              => NoChange("in a method and not visible")
        case _: ObjectModel                => NoChange("object is effectively final")
        case _: VarModel                   => NoChange("var is always final")
        case _ if (element.isFinal)        => NoChange("its already final")
        case _
            if element.isPrivate
            //fieds are marked private, so exclude them and handle in calcFieldLevel
              && !element.isInstanceOf[FieldModel] =>
          NoChange("its private so probably not worth the change")
        case fieldsModel: FieldsModel if (fieldsModel.fields.exists(_.isInstanceOf[VarModel])) =>
          NoChange("vars are always final")
        case fieldModel: FieldModel if fieldModel.inCompoundFieldDeclaration => NoChange("part of a compound decl")

        case fieldsModel: FieldsModel => calcFieldsLevel(fieldsModel)
        case methodModel: MethodModel => calcMethodLevel(methodModel)
        case fieldModel: FieldModel   => calcFieldLevel(fieldModel)
        case clsOrTrait: ClassLike    => calcClassLevel(clsOrTrait)
      }
      element.colour = colour
    }
    element.colour
  }

  def calcMethodLevel(method: MethodModel): Colour = {
    method.enclosing.head match {
      case cls: ClassModel if localLevel(cls) == Final => NoChange("owner is a final class")
      case cls: ObjectModel                            => NoChange("owner is a an object")
      case cls: ClassLike =>
        val overrides = method.overridden
        if (overrides isEmpty) Final
        else Open("")
      case encl => NoChange(s"enclosed in $encl")
    }

  }

  def calcFieldLevel(field: FieldModel): Colour = {
    field.declaredIn.getOrElse(field).enclosing.head match {
      case cls: ClassModel if cls.isFinal || cls.isPrivate => NoChange("owner is private/final")
      case cls: ClassModel if localLevel(cls) == Final     => NoChange("owner will be a final class")
      case cls: ObjectModel                                => NoChange("owner is a an object (so final)")
      case cls: ClassLike                                  =>
        // we need to consider the accessors. The val isn't overidded, but if the accessor is then we cant make it final
        // in the parent
        val overrides: Iterable[Overrides] = field.overridden ++ field.accessors.flatMap(_.overridden)
        if (overrides isEmpty)
          Final
        else {
          val size = overrides.size
          NoChange(s"$size overrides - e.g. ${overrides.head.fromElementId}")
        }
      case encl => NoChange(s"enclosed in $encl")
    }
  }

  def calcFieldsLevel(fieldsModel: FieldsModel): FinaliserLevel = {
    fieldsModel.fieldsInDeclaration.foldLeft(Undefined: FinaliserLevel) { case (level, field) =>
      level.widen(calcFieldLevel(field))
    }
  }

  @tailrec final def getSource(element: ModelElement): SourceModel = element match {
    case sourceModel: SourceModel => sourceModel
    case _                        => getSource(element.enclosing.head)
  }

  def calcClassLevel(classLike: ClassLike): FinaliserLevel = classLike match {
    case model: ClassModel =>
      val ext = model.extendedByClassLike()
      if (ext isEmpty)
        Final
      else {
        val mySource = getSource(model)
        if (ext.forall(cls => getSource(cls) == mySource))
          Sealed("")
        else
          Open("")
      }
    case model: ObjectModel => NoChange("objects are final")
    case model: TraitModel =>
      val mySource = getSource(model)
      val ext      = model.extendedByClassLike()
      if (ext.forall(cls => getSource(cls) == mySource))
        Sealed("")
      else
        Open("")

  }
//
//  var elementsObserved = 0
//  var elementsChanged  = 0
//
//  override def printSummary(projectName: String): Unit = println(s"""Elements Observed = $elementsObserved
//                                                                    |Elements Changed  = $elementsChanged
//                                                                    |Effect rate       = ${(elementsChanged.toDouble / elementsObserved.toDouble * 10000).toInt / 100} %"
//                                                                    |""".stripMargin)

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

      def handleDecl(modelElement: ModelElement) = {
        modelElement.colour match {
          case Open(reason)     =>
          case NoChange(reason) =>
          case Undefined        => ???
          case Sealed(reason)   =>
          case Final =>
            if ((Flags.FINAL & modelElement.flags) == 0)
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "final ", ""))
        }
      }
      def handleClass(modelElement: ModelElement) = {
        modelElement.colour match {
          case Open(reason)     =>
          case NoChange(reason) =>
          case Undefined        => ???
          case Sealed(reason) =>
            val isSealed = (Flags.SEALED & modelElement.flags) != 0
            if (!isSealed)
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "sealed ", ""))
          case Final =>
            val isFinal  = (Flags.FINAL & modelElement.flags) != 0
            val isSealed = (Flags.SEALED & modelElement.flags) != 0
            //TODO cope with sealed -> final
            if (!isFinal && !isSealed)
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "final ", ""))
        }
      }

      override protected def visitInSource(modelElement: ModelElement): Boolean = {
        modelElement match {
          case fieldModel: FieldModel if fieldModel.declaredIn.nonEmpty      =>
          case accessorModel: AccessorModel if accessorModel.field.isDefined =>
          case _: MethodModel | _: FieldModel | _: FieldsModel =>
//            elementsObserved += 1
            handleDecl(modelElement)
          case classLike: ClassLike =>
//            elementsObserved += 1
            handleClass(modelElement)
        }
        true
      }
    }
    visitor.visit(sModel)

    val result = visitor.result
//    elementsChanged += result.size
//    if (debug) {
//      println("--------NEW----------")
//      result.foreach(println)
//      println("------------------")
//      }
    result
  }

}
class FinaliserCommandLine              extends ScalaCleanCommandLine
