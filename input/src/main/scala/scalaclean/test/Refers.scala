/*
rules = [ Analysis , TestRefers ]

*/
package scalaclean.test

class Parent {
  def foo: Unit = ()
}
class Child extends Parent{
  def bar(x:Any): Unit = ()
}

object XX {
  new Child().foo
  new Child().bar(1)
}