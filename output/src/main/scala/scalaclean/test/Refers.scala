package scalaclean.test

class Parent {
  def foo: Unit = ()/* internalIncomingReferences(scalaclean/test/Parent#foo().) - scalaclean/test/XX. */
}/* internalIncomingReferences(scalaclean/test/Parent#) - scalaclean/test/Child# */
class Child extends Parent{
  def bar(x:Any): Unit = ()/* internalIncomingReferences(scalaclean/test/Child#bar().) - scalaclean/test/XX. */
}/* internalIncomingReferences(scalaclean/test/Child#) - scalaclean/test/XX. */

object XX {
  new Child().foo
  new Child().bar(1)
}