package scalaclean.rules

import scalaclean.model._
import scalafix.v1.{SemanticDocument, Symbol, SyntacticDocument}

import scala.meta.io.AbsolutePath

abstract class AbstractRule(val name: String, val model: ProjectModel, debug: Boolean) {
  def printSummary(projectname: String): Unit

  type Colour <: Mark

  final def beforeStart(): Unit = {
    if (debug)
      println(s"$name performing analysis")

    markInitial()

    runRule()

    if (debug)
      debugDump()

    if (debug)
      println(s"$name analysis complete")
  }

  def debugDump(): Unit = {}

  def markInitial(): Unit

  def runRule(): Unit

  def fix(targetFile: AbsolutePath, syntacticDocument: SyntacticDocument)(implicit semanticDocument: SemanticDocument): List[SCPatch]

  def markAll[T <: ModelElement : Manifest](colour: => Colour): Unit = {
    model.allOf[T].foreach {
      e => e.mark = colour
    }
  }

  implicit class Coloured(e: ModelElement) {
    def colour: Colour = e.mark.asInstanceOf[Colour]

    def colour_=(newColour: Colour): Unit = e.mark = newColour
  }

  //utility methods

  def allMainEntryPoints = {
    allMainMethodEntries ++ allApp
  }

  def allMainMethodEntries: Iterator[ModelElement] = {
    model.allOf[ObjectModel].filter(_.isTopLevel).flatMap { om =>
      om.methods.collect { case mm: PlainMethodModel if(mm.methodName == "main") => mm }
    }
  }

  def allApp: Iterator[ObjectModel] = {
    val allAppObjects = ElementIds.allAppObjects
    for (obj <- model.allOf[ObjectModel] if allAppObjects.exists(appLike => obj.xtends(appLike)))
      yield obj

  }

  def allTestEntryPoints = {
    allJunitTest
  }

  private val junitAnnotationEntryPoints = Set(
    "org.junit.Test",
    "org.junit.Before",
    "org.junit.After",
    "org.junit.BeforeClass",
    "org.junit.AfterClass",
  )

  def allJunitTest = {
    model.allOf[MethodModel].filter { method =>
      method.annotations.exists { a =>
        junitAnnotationEntryPoints.contains(a.fqName)
      }
    }
  }

}
