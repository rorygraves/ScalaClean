package scalaclean.test.rules.privatiser.p1

object Private1 extends App {
  //some private[this] elements

  private[this] val a = 1
  private[this] def b = 2
  private[this] var c = 3

  a+b+c


  private[this] val d = 1
  private[this] def e = 2
  private[this] var f = 3
  d+e+f
}

