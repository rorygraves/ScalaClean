package selftype

object SelfType extends App {
  val y1 = new Y1 with X1
  val y2 = new Y2 with X2
  y1.x()
  y2.x()
}

trait X1
trait Y1 {
  x: X1 =>
  def x() = ???
}
trait X2
class Y2 {
  x: X2 =>
  def x() = ???
}