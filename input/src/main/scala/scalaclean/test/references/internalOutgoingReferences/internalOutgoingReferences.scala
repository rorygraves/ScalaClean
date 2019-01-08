/*
rules = [ Analysis , Test_internalOutgoingReferences ]

*/
package scalaclean.test.references.internalOutgoingReferences

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