package scalaclean.test.rules.deadcode.varval

object App1 extends App {
  Outer.Inner
  ()
}
object Outer {
  object Inner {
    var r20 = referred20
    val r21 = referred21
    lazy val r22 = referred22

    var (_,_,_, (d2,_)) = (1,2,3,(4,referred2))

    val (_,_,_, (d4,_)) = (1,2,3,(4,referred4))

    lazy val (_,_,_, (d6,_)) = (1,2,3,(4,referred6))
    d2
    d4
    d6
    r20
    r21
    r22
    ()

  }
  object referred10
  object referred11
  object referred20
  object referred21
  object referred22

  object referred1
  object referred2
  object referred3
  object referred4
  object referred6
}
