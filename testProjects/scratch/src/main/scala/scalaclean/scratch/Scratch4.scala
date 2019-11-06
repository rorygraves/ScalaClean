package scalaclean.scratch

import scala.annotation.tailrec

object ScratchObj {
  var field = 1
}

class Scratch4 {

  private def withTransportInformation[T](f: () ⇒ T): T = {
    val oldInfo = ???
    try {
      f()
    } finally ???
  }

  def myMethod(x: Int): Unit = {
    @tailrec
    def recursive(x: Int): Int = {
      if (x > 0)
        recursive(x - 1)
      else
        0
    }

    withTransportInformation { () ⇒
      1 match {
        case 1 =>
        case 2 ⇒
          Option("abc") match {
            case Some(cachedClassManifest) ⇒
            case None ⇒
              recursive(5)
            //                system.dynamicAccess.getClassFor[AnyRef](manifest) match {
            //                  case Success(classManifest) ⇒
            //                    val classManifestOption: Option[Class[_]] = Some(classManifest)
            //                    updateCache(cache, manifest, classManifestOption)
            //                    s1.fromBinary(bytes, classManifestOption)
            //                  case Failure(_) ⇒
            //                    throw new NotSerializableException(
            //                      s"Cannot find manifest class [$manifest] for serializer with id [${serializer.identifier}].")
            //                }
          }
      }
    }
  }
}
