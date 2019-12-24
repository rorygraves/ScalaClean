package scalaclean.test.deadcode6a

// Used by deadcode6b - must be tested in conjunction
class DC6UsedClass {
  def usedMethod(s: String): Unit = {
    println(s)
  }

  def unusedMethod(d: Double): Unit = {
    println(d)
  }
}
