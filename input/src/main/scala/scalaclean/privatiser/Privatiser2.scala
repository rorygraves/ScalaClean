/*
rules = [ ScalaCleanPrivatiserAnalysis , ScalaCleanPrivatiserApply ]

*/
package scalaclean.privatiser

object PRef1 {

  def foo(): Unit = {
    bar(2)
  }

  def foo(x : Int): Unit = {
    bar(3)
  }

  def foo(y : String): Unit = {
    bar(4)
  }

  def foo(x: List[Int]): Unit = {
    println(x)
  }

  // should be private
  def bar(x: Int): Unit = {

  }

  // should be private
  object Baz

}

object PMain {
  def main(args: Array[String]): Unit = {
    val y = 7
    PRef1.foo(y)
    PRef1.foo("YYYY")
    PRef1.foo(List(1,2,3,4))
  }
}
