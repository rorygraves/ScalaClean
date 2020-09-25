package scalaclean.test.rules.finaliser.p1

object Final1  {
  // some final elements, but should not be marked as owner is already

  val a = 1
  def b = 2
  var c = 3
  a+b+c


}

class ShouldBeFinal {
  //not final as enclosing is
  def a = 1
  val b = 1
  var c = 1


  private class X
  private object Y
}
abstract class NotFinalClass {
  def a = 1
  val b = 1
  var c = 1

  def fa = 1
  val fb = 1
  //vars are final
  var fc = 1

  def a1: Int
  val b1: Int
  var c1: Int

  private class X
  private object Y
}
trait NotFinalTrait {
  def a = 1
  val b = 1
  var c = 1

  def a1: Int
  val b1: Int
  var c1: Int

  private class X
  private object Y
}

class ExtNotFinalTrait extends NotFinalTrait {

  override def a = 1
  override val b = 1

  override def a1: Int = 1
  override val b1: Int = 1
  override var c1: Int = 1
}
class ExtNotFinalClass extends NotFinalClass {
  override def a = 1
  override val b = 1

  override def a1: Int = 1
  override val b1: Int = 1
  override var c1: Int = 1
}