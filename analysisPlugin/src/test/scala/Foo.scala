
class Foo {
  var x = 1
  def bar(y : Int): Unit = {
    println("Hello" + y)
  }

  def bas(): Unit = {
    bar(4)
  }

  def baz(x: String): Unit = {
    println("Hello")
  }
}

object AppWithMain extends App {
  val foo = new Foo
  println(foo.bas())
}

object AppWithMain2 {
  def main(): Unit = {
    val foo = new Foo
    println(foo.bas())
  }
}