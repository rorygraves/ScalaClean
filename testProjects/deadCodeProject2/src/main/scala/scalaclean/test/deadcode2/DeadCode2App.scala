package scalaclean.test.deadcode2

object DeadCode2App extends App {

  val x = Map.empty[Int,String]
  println(x)
  foo()

  def foo(): Unit = {
    println("BAR")
  }
}
