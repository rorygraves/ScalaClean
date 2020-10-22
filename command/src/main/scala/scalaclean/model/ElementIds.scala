package scalaclean.model

object ElementIds extends ElementIdManager {
  val AppObject: ElementId = ElementIds.forClass[App]

  val allAppObjects: List[ElementId] = List(AppObject)
}
