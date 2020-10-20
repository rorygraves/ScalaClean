package pkg

object Entry extends App {
  println(new BaseClass1().toString)
  println(new BaseClass2("").x)

  println(new AbstractClass1(){}.toString)
  println(new AbstractClass2(""){}.x)

  println(new Trait1{}.toString)
  println(new Trait2{val x = ""}.x)

}

// no changes expected
class BaseClass1
class BaseClass2(val x:String)

abstract class AbstractClass1
abstract class AbstractClass2(val x:String)

trait Trait1
trait Trait2{val x:String}

