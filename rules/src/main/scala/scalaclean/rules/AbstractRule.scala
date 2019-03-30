package scalaclean.rules

import scalaclean.model._
import scalafix.v1.SemanticRule

abstract class AbstractRule(name:String) extends SemanticRule(name) {
  var model: ProjectModel = _
  type Colour <: Mark

  final override def beforeStart(): Unit = {
    println(s"$name start beforeStart")
    // load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))

    markInitial

    runRule()

    println(s"$name end beforeStart")
  }
  def markInitial(): Unit

  def runRule() : Unit

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
