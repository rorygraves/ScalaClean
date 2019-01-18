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
}

object App1 extends App {
  def main(): Unit = {
    val used = Used3
    println(used.aMethod())
  }
}
object Used3 {
  def aMethod(): Unit = {}
}
object Outer {
  object Inner {
    lazy val (_,_,_, (d3,_)) = (1,2,3,(4,referred3))
    d3
  }
  object referred1
  object referred2
  object referred3
}
