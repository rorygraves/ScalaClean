package scalaclean.test.rules.deadcode

object DeadCodePackageVals extends App {
  Outer.Inner
}

object Outer {
  object Inner {

    var r1 = referred1
    println(r1)
  }

  object referred1

  object unReference1
}
