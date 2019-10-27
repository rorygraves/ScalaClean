package scalaclean.scratch

object ScratchObj {
  var field = 1
}

class Scratch4 {

  def myMethod(x: Int): Unit = {
    val z = x
    ScratchObj.field = z + 1
  }
}
