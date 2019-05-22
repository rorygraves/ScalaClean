package scalaclean.test.rules.privatiser.p2

object Private2 extends App {

  //not used element should be flagged with comments
  def notUsed1 = ???

  val notUsed2 = ???
  var notUsed3 = ???

  Other.used
  Other.usedAndVisible1
  new Other.usedAndVisible2
  new Other.usedAndVisible3{}
}
object Other {

  def used = {
    //content should be unaffected inside a def
    //as the values are not visible
    val x = 1
    var y = 1
    def z = 1

    val a = 1
    var b = 1
    def c = 1

    x+y+z
  }

  object usedAndVisible1 extends AnyRef {
    //content should be affected inside an object
    //as the values are not visible
    val x = 1
    var y = 1
    def z = 1

    val a = 1
    var b = 1
    def c = 1

    x+y+z
  }

  class usedAndVisible2 extends AnyRef {
    //content should be affected inside a class
    //as the values are not visible
    val x = 1
    var y = 1
    def z = 1

    val a = 1
    var b = 1
    def c = 1

    x+y+z
  }

  trait usedAndVisible3 extends AnyRef {
    //content should be affected inside a trait
    //as the values are not visible
    val x = 1
    var y = 1
    def z = 1

    val a = 1
    var b = 1
    def c = 1

    x+y+z
  }

  //some declarations wit internal structure
  //should not be traversed as the content is not visible

  val a = {
    val x = 1
    def y = 1
    var z = 0
    x+y+z
  }
  def b = {
    val x = 1
    def y = 1
    var z = 0
    x+y+z
  }
  var c = {
    val x = 1
    def y = 1
    var z = 0
    x+y+z
  }
  a+b+c
}
