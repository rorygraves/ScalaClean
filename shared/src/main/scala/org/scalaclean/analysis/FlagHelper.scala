package org.scalaclean.analysis

import scala.reflect.internal.Flags

object FlagHelper {
  def hexAndString(flags:Long, sep:String = ";"): String = {
    s"${java.lang.Long.toHexString(flags)} - ${flagsToString(flags,sep)}"
  }
  def flagsToString(flags:Long, sep:String = ","): String = {
    val b = new StringBuilder
    var remaining = flags
    while (remaining != 0) {
      val flag = java.lang.Long.lowestOneBit(remaining)
      remaining = remaining ^ flag
      b.append(Flags.flagToString(flag))
      if (remaining != 0)
        b.append(sep)
    }

    b.toString()
  }

}
