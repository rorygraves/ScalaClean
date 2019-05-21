package scalaclean.rules

import scalaclean.model._
import scalafix.patch.Patch
import scalafix.v1.SemanticDocument

abstract class AbstractRule(val name:String) {
  var model: ProjectModel = _
  type Colour <: Mark

  final def beforeStart(): Unit = {
    println(s"$name start beforeStart")
    // load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))

    markInitial()

    runRule()

    println(s"$name end beforeStart")
  }
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
