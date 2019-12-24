package scalaclean.test.rules.privatiser.p1

object Private1 extends App {
  // some private[this] elements
  // can't detect [this] though so should be private
  // where it isnt [this] already

  val a = 1
  def b = 2
  var c = 3
  a+b+c


  private[Private1] val d = 1
  private[test] def e = 2
  private var f = 3
  private[this] def h = 3
  d+e+f+h

  protected[Private1] val l = 1
  protected[test] def m = 2
  protected var n = 3
  protected[this] def o = 3
  l+m+n+o
}
