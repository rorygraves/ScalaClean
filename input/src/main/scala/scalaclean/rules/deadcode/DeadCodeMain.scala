/*
rules = [ Analysis , ScalaCleanDeadCodeRemover ]

*/
package scalaclean.rules.deadcode.deadmain

import scalaclean.rules.deadcode.deadmain.Outer.Inner3

object AppWithMain1 {
  def main(args: Array[String]): Unit = {
    val used = Used1
    println(used.aMethod())
    Outer.Inner1
    ()
  }
}
object Used1 {
  def aMethod(): Unit = {}
  def aMethod(notUsed:Int): Unit = {}
  def notUsedMethod(): Unit = {}
}

object AppWithMain2 {
  import Outer._
  def main(): Unit = {
    val used = Used2
    println(used.aMethod())
    Inner2
    ()
  }
}
object Used2 {
  def aMethod(): Unit = {}
  def aMethod(notUsed:Int): Unit = {}
  def notUsedMethod(): Unit = {}
}

object App1 extends App {
  val used = Used3
  println(used.aMethod())
  Inner3
  ()
}
object Used3 {
  def aMethod(): Unit = {}
  def aMethod(notUsed:Int): Unit = {}
  def notUsedMethod(): Unit = {}
}

object Outer {
  //referenced by AppWithMain1
  object Inner1
  //referenced by AppWithMain2
  object Inner2
  //referenced by App1
  object Inner3

  object NotUsed
}
case class UnusedClass(value: String)
