package scalaclean.model

object ElementIdM extends ElementIdManager
object ElementIds {
  val AppObject: ElementId = ElementIdM.forClass[App]

  val allAppObjects: List[ElementId] = List(AppObject)
}
