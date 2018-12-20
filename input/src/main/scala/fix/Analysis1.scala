/*
rules = [ ScalaCleanAnalysis , ScalaCleanDeadClass ]

*/
package fix

object TestMain {
  def main(args: Array[String]) = {

    val used = UsedClass(5)
    println(used.usedMethod(true, 3))
  }

}

case class UsedClass(value: Int)  {
  def usedMethod(myBool: Boolean, y: Int) : Int = {
    y + value
  }

  def unusedMethod(z: Int): String = {
    ???
  }

}

case class UnusedClass(value: String)
