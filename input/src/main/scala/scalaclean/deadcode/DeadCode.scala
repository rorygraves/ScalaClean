/*
rules = [ Analysis , ScalaCleanDeadCodeRemover ]

*/
package scalaclean.deadcode.dead1

object AppWithMain1 {
  def main(args: Array[String]): Unit = {
    val used = Used1
    println(used.aMethod())
  }
}
object Used1 {
  def aMethod(): Unit = {}
  def aMethod(notUsed:Int): Unit = {}
  def notUsedMethod(): Unit = {}
}

object AppWithMain2 {
  def main(): Unit = {
    val used = Used2
    println(used.aMethod())
  }
}
object Used2 {
  def aMethod(): Unit = {}
  def aMethod(notUsed:Int): Unit = {}
  def notUsedMethod(): Unit = {}
}

object App1 extends App {
  def main(): Unit = {
    val used = Used3
    println(used.aMethod())
  }
}
object Used3 {
  def aMethod(): Unit = {}
  def aMethod(notUsed:Int): Unit = {}
  def notUsedMethod(): Unit = {}
}


case class UnusedClass(value: String)