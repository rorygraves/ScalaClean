package scalaclean.rules

import scalaclean.model._
import scalaclean.model.impl.LegacyElementId
import scalafix.patch.Patch
import scalafix.v1.{SemanticDocument, Symbol}

abstract class AbstractRule(val name: String, val model: ProjectModel, debug: Boolean) {
  def printSummary: Unit

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

  def fix(implicit doc: SemanticDocument): Patch

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

  def allMainMethodEntries = {
    val stringArray = List(List(Symbol))

    (for (obj <- model.allOf[ObjectModel] if (obj.isTopLevel);
          method <- obj.methods if method.name == "main") //&& method.paramsType = stringArray
      yield {
        List(method, obj)
      }).flatten
  }

  def allApp = {
    val app = ElementIds.AppObject
    for (obj <- model.allOf[ObjectModel] if (obj.xtends(app)))
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
      method.annotations.exists{ a =>
        junitAnnotationEntryPoints.contains(a.fqName)}
    }
  }

}
