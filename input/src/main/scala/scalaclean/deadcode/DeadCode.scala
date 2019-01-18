/*
rules = [ Analysis , ScalaCleanDeadCodeRemover ]

*/
package scalaclean.deadcode.dead1

object AppWithMain1 {
  def main(args: Array[String]): Unit = {
    val used = Used1
    println(used.aMethod())
    Outer
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
    Inner
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

object Outer {
  object Inner {
    val r2 = referred2
    var (a,b,c, (d,e)) = (1,2,3,(4,referred1))
    lazy val (a3,b3,c3, (d3,e3)) = (1,2,3,(4,referred3))
    lazy val (a4,b4,c4, (d4,e4)) = (1,2,3,(4,referred4))
    d3
  }
  object NotUsed
  object referred1
  object referred2
  object referred3
  object referred4
}
case class UnusedClass(value: String)

@deprecated
case class UnusedClassWithAnnotation(value: String)
/** some docs */
object UnusedObjectDoc