package scalaclean.test.deadcode2

object DeadCode2App {

  def main(args: Array[String]): Unit = {
    val used = UsedObj
    println(used.usedMethod())
  }

}

object UsedObj {
  def usedMethod(): Int = { 1 }
  private def notUsedMethod(): Unit = {}
}
