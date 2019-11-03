package scalaclean.rules

import scalaclean.model._
import scalaclean.model.impl.ElementId
import scalafix.patch.Patch
import scalafix.v1.{SemanticDocument, Symbol}

abstract class AbstractRule(val name:String, val model: ProjectModel, debug: Boolean) {
  type Colour <: Mark

  final def beforeStart(): Unit = {
    if(debug)
      println(s"$name performing analysis")

    markInitial()

    runRule()

    if(debug)
      debugDump()

    if(debug)
      println(s"$name analysis complete")
  }

  def debugDump(): Unit = {}

  def markInitial(): Unit

  def runRule() : Unit

  def fix(implicit doc: SemanticDocument): Patch

  def markAll[T <: ModelElement: Manifest](colour: => Colour): Unit = {
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
    val app = ElementId.AppObject
    for (obj <- model.allOf[ObjectModel] if (obj.xtends(app)))
      yield obj
  }

  def allTestEntryPoints = {
    allMainMethodEntries ++ allApp
  }
  def allJunitTest = {
    model.allOf[MethodModel] collect {
      case method if (method.annotations.exists(_.fqName == "org.junit.Test")) => method
    }
  }

}
