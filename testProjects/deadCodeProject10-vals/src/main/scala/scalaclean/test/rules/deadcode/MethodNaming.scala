
trait t1 {
  def m1: Int
}

trait t2 extends t1{
  val m1: Int
}

trait t3 extends t1{
  val m1: Int = 1
}

abstract class c1 extends t1{
  val m1: Int
}

class c2 extends t1{
  val m1: Int = 7
}


