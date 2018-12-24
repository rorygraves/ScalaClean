/*
rules = [ ScalaCleanPrivatiserAnalysis , ScalaCleanPrivatiserApply ]

*/
package scalaclean.privatiser

object PRef1 {

  def foo(): Unit = {
    bar()
  }

  // should be private
  def bar(): Unit = {

  }
}

object PMain {
  def main(args: Array[String]): Unit = {
    PRef1.foo()
  }
}