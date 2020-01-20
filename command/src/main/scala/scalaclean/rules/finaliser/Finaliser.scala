package scalaclean.rules.finaliser

import scalaclean.model._
import scalaclean.rules.AbstractRule
import scalaclean.util.ScalaCleanTreePatcher
import scalafix.v1.SyntacticDocument

import scala.annotation.tailrec
import scala.meta.io.AbsolutePath
import scala.reflect.internal.Flags

//marks things final and sealed were it can
class Finaliser(model: ProjectModel, debug: Boolean) extends AbstractRule("Finaliser", model, debug) {

  type Colour = FinaliserLevel

  override def markInitial(): Unit = {
    model.allOf[ModelElement].foreach { e =>
      e.colour = Undefined
    }
  }

  override def runRule(): Unit = {
    model.allOf[ModelElement].foreach {
      case e: SourceModel =>
        e.colour = NoChange("source")
      case e => e.colour = localLevel(e)
    }
    model.allOf[ModelElement].toList.sortBy(_.infoPosSorted).foreach(ele =>
      println(s"$ele  colour: ${ele.colour}"))
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
//        case x if x.modelElementId.isLocal => NoChange("its local")
        case x if !x.existsInSource => NoChange("no source")
        case _: SourceModel => NoChange("source")
//        case x if inMethod(x) => NoChange("in a method and not visible")
        case fieldsModel: FieldsModel => calcFieldsLevel(fieldsModel)
        case fieldModel: FieldModel if fieldModel.inCompoundFieldDeclaration => localLevel(fieldModel.declaredIn.get)
        case getterMethodModel: GetterMethodModel => localLevel(getterMethodModel.field.get)
        case setterMethodModel: SetterMethodModel => localLevel(setterMethodModel.field.get)
        case fieldModel: FieldModel => calcFieldLevel(fieldModel)
        case _ =>
          calcSingleLevel(element)
      }
      element.colour = colour
    }
    element.colour
  }

  def calcFieldLevel(field: FieldModel): Colour = {
    field.accessors.foldLeft(calcSingleLevel(field)) {
      case (level, accessor) => level.widen(calcSingleLevel(accessor))
    }
  }

  def calcFieldsLevel(fieldsModel: FieldsModel): FinaliserLevel = {
    fieldsModel.fieldsInDeclaration.foldLeft(Undefined: FinaliserLevel) {
      case (level, field) => level.widen(calcFieldLevel(field))
    }
  }
  @tailrec final def getSource(element: ModelElement): SourceModel =
    element match {
      case sourceModel: SourceModel => sourceModel
      case _ => getSource(element.enclosing.head)
    }

  def calcSingleLevel(element: ModelElement): FinaliserLevel = {
    val overrides = element.overrides
    val mySource = getSource(element)
    if (overrides isEmpty) Final
    else if (overrides forall { o  => getSource(o.fromElement) == mySource}) Sealed("")
    else Open("")
  }

  var elementsObserved = 0
  var elementsChanged = 0

  override def printSummary(projectName: String): Unit =
    println(
      s"""Elements Observed = $elementsObserved
         |Elements Changed  = $elementsChanged
         |Effect rate       = ${(elementsChanged.toDouble / elementsObserved.toDouble * 10000).toInt / 100} %"
         |""".stripMargin)

  override def fix(targetFile: AbsolutePath, syntacticDocument: () => SyntacticDocument): List[SCPatch] = {
    val targetFileName = targetFile.toString
    // find source model
    val sModel = model.allOf[SourceModel].filter(_.toString.contains(targetFileName)).toList.headOption.getOrElse(throw new IllegalStateException(s"Unable to find source model for $targetFileName"))

    object visitor extends ScalaCleanTreePatcher(patchStats, syntacticDocument) {

      def handleDecl(modelElement: ModelElement) = {
        modelElement.colour match {
          case Open(reason) =>
          case NoChange(reason) =>
          case Undefined => ???
          case Sealed(reason) =>
          case Final =>
            if ((Flags.FINAL & modelElement.flags) == 0)
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "final ", ""))
        }
      }
      def handleClass(modelElement: ModelElement) = {
        modelElement.colour match {
          case Open(reason) =>
          case NoChange(reason) =>
          case Undefined => ???
          case Sealed(reason) =>
          case Final =>
            val isFinal = (Flags.FINAL & modelElement.flags) != 0
            val isSealed = (Flags.SEALED & modelElement.flags) != 0
            
              collect(SCPatch(modelElement.rawStart, modelElement.rawStart, "final ", ""))
        }
      }

      override protected def visitInSource(modelElement: ModelElement): Boolean = {
        modelElement match {
          case fieldModel: FieldModel if fieldModel.declaredIn.nonEmpty =>
          case accessorModel: AccessorModel if accessorModel.field.isDefined =>
          case _: MethodModel | _: FieldModel | _: FieldsModel =>
            elementsObserved += 1
            handleDecl(modelElement)
          case classLike: ClassLike =>
            elementsObserved += 1
            handleClass(modelElement)
        }
        true
      }
    }
    visitor.visit(sModel)

    val result = visitor.result
    println("--------NEW----------")
    result.foreach(println)
    println("------------------")

    result
  }
}
