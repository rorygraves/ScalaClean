package scalaclean.model

object ElementIds {
  val AppObject: ElementId = ElementId.forClass[App]

  val allAppObjects :List[ElementId] = List(AppObject)
}
