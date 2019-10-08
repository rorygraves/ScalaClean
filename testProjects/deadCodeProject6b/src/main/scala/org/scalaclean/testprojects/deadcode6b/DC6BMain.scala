package org.scalaclean.testprojects.deadcode6b

import org.scalaclean.testprojects.deadcode6a.DC6UsedClass

object DC6BMain {
  def main(args: Array[String]): Unit = {
    println("Hello world")
    new DC6UsedClass().usedMethod("a")
  }
}
