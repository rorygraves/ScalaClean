package scalaclean.rules

import scalaclean.model._
import scalafix.patch.Patch
import scalafix.v1.SemanticDocument

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

  def markAll[T <: ModelElement: Manifest](colour: => Colour) = {
    model.allOf[T].foreach {
      e => e.mark = colour
    }
  }
  implicit class Coloured(e: ModelElement) {
    def colour = e.mark.asInstanceOf[Colour]
    def colour_=(newColour: Colour) = e.mark = newColour
  }
}
