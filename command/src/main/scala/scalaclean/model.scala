package scalaclean

import scala.annotation.{implicitAmbiguous, implicitNotFound}

package object model {
  @implicitAmbiguous("you need to supply an actual type, not Nothing")
  @implicitNotFound("you need to supply an actual type")
  sealed trait NotNothing[-T]
  implicit object notNothing extends NotNothing[Any]
  implicit object notNothing1 extends NotNothing[Nothing]
}
