package classparams

object Entry extends App {
  println(Case1)
  Case2(1,2,3)
  new Case3(1,2,3)

  new Class1(1,2,3)
  new Class2(1,2,3)

  new ClassWithUnused(1,2,3)
  CaseWithUnused(1,2,3)

  foo(1,2,3)

  def foo(a:Int, b:Int,c:Int) = ???
}

case class Case1(a: Int,b: Int,c: Int)
case class Case2(a: Int,b: Int,c: Int)
case class Case3(a: Int,b: Int,c: Int)
case class CaseDelete(a: Int,b: Int,c: Int)

class Class1(a: Int,b: Int,c: Int)
class Class2(val a: Int,val b: Int,val c: Int)
class ClassDelete(a: Int,b: Int,c: Int)

case class CaseWithUnused(a: Int,b: Int,c: Int) {
  val deleteMe = 1
}
class ClassWithUnused(a: Int,b: Int,c: Int) {
  val deleteMe = 1
}
