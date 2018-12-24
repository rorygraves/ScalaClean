package scalaclean.deadcode

object DeadClass2Main extends App {

  val cl = new ImplClass
  println("cl.foo=" + cl.foo)
}

trait Trait1 {
  val foo = 7
}

trait UnusedTrait {
  val bar = 6
}

class ImplClass extends Trait1 {

}