/*
rules = [ Analysis , Test_internalIncomingReferences ]

*/
package scalaclean.test.references.internalIncomingReferences

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