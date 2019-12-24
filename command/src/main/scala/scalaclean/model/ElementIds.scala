package scalaclean.model

import scalaclean.model.impl.NewElementIdImpl

object ElementIds {
  //FIXME
  val AppObject: NewElementId = NewElementIdImpl("<root>/scala@/App.")

  val allAppObjects :List[NewElementId] = List(AppObject)
}
