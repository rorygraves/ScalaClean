package scalaclean.model

import scalaclean.model.ElementId

object ElementIds {
  //FIXME
  val AppObject: ElementId = ElementId.forClass[App]

  val allAppObjects :List[ElementId] = List(AppObject)
}
