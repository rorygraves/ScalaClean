/*
rules = [ Analysis , ScalaCleanDeadCodeRemover ]

*/
package scalaclean.deadcode.varval

object App1 extends App {
  Outer.Inner
}
object UnUsed3 {
  def aMethod(): Unit = {}
  def aMethod(notUsed:Int): Unit = {}
  def notUsedMethod(): Unit = {}
}

object Outer {
  object Inner {
    var r10 = referred10
    val r11 = referred11
    lazy val r12 = referred12

    var r20 = referred20
    val r21 = referred21
    lazy val r22 = referred22

    var (a1,b1,c1, (d1,e1)) = (1,2,3,(4,referred1))
    var (a2,b2,c2, (d2,e2)) = (1,2,3,(4,referred2))

    val (a3,b3,c3, (d3,e3)) = (1,2,3,(4,referred3))
    val (a4,b4,c4, (d4,e4)) = (1,2,3,(4,referred4))

    lazy val (a5,b5,c5, (d5,e5)) = (1,2,3,(4,referred5))
    lazy val (a6,b6,c6, (d6,e6)) = (1,2,3,(4,referred6))
    d2
    d4
    d6
    r20
    r21
    r22

  }
  object NotUsed
  object referred10
  object referred11
  object referred12

  object referred20
  object referred21
  object referred22

  object referred1
  object referred2
  object referred3
  object referred4
  object referred5
  object referred6
}
