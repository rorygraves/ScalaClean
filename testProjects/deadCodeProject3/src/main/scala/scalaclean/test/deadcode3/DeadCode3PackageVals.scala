package scalaclean.test.deadcode3

object DeadCodePackageVals extends App {
  Outer.Inner
}

object Outer {

  object Inner {

    var r1 = referred1
    println(r1)
  }

  object referred1

  object referred2

  println(referred2)

  object unReference1
}
