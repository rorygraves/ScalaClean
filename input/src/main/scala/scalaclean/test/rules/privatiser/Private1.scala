/*
rules = [ Analysis , Privatiser ]

*/
package scalaclean.test.rules.privatiser.p1

object Private1 extends App {
  //some private[this] elements
  //can't detect [this] though so should be private

  val a = 1
  def b = 2
  var c = 3
  a+b+c


  private[Private1] val d = 1
  private[test] def e = 2
  private var f = 3
  d+e+f
}
