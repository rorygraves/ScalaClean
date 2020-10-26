package defaultparams

object Entry extends App {
  println(Case1)
  Case2(1,2)
  new Case3(1,2)

  new Class1(1,2)
  new Class2(1,2)

  new ClassWithUnused(1,2)
  CaseWithUnused(1,2)

  foo1(1)
  foo2(1,2)
  foo3(1,2,3)

  def foo1(a:Int, b:Int = 1,c:Int = 2) = ???
  def foo2(a:Int, b:Int = 1,c:Int = 2) = ???
  def foo3(a:Int, b:Int = 1,c:Int = 2) = ???
}

case class Case1(a: Int,b: Int,c: Int = 1)
case class Case2(a: Int,b: Int,c: Int = 1)
case class Case3(a: Int,b: Int,c: Int = 1)
case class CaseDelete(a: Int,b: Int,c: Int = 1)

class Class1(a: Int,b: Int,c: Int = 1)
class Class2(val a: Int,val b: Int,val c: Int = 1)
class ClassDelete(a: Int,b: Int,c: Int = 1)

case class CaseWithUnused(a: Int,b: Int,c: Int = 1) {
  val deleteMe = 1
}
class ClassWithUnused(a: Int,b: Int,c: Int = 1) {
  val deleteMe = 1
}
