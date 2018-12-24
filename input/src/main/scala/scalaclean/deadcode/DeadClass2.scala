/*
rules = [ ScalaCleanDeadCodeAnalysis , ScalaCleanDeadCodeRemover ]

*/
package scalaclean.deadcode

object DeadClass2Main extends App {

  val cl = new ImplClass
  println("cl.foo=" + cl.foo)
}

trait Trait1 {
  val foo = 7
  def unusedMethod(x: Int): Int = { x + 1 }
}

trait UnusedTrait {
  val bar = 6
}

class ImplClass extends Trait1 {

}

class UnusedImplClass extends Trait1 {

}