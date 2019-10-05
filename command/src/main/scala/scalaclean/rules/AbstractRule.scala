package scalaclean.rules

import scalaclean.model._
import scalafix.patch.Patch
import scalafix.v1.SemanticDocument

abstract class AbstractRule(val name:String, val model: ProjectModel) {
  type Colour <: Mark

  final def beforeStart(): Unit = {
    println(s"$name start beforeStart")

    markInitial()

    runRule()

    debugDump()

    println(s"$name end beforeStart")


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
