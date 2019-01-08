package scalaclean.test.references.internalOutgoingReferences

class Parent {
  def foo: Unit = ()
}
class Child extends Parent{
  def bar(x:Any): Unit = ()
}/* internalOutgoingReferences(scalaclean/test/references/internalOutgoingReferences/Child#) - scalaclean/test/references/internalOutgoingReferences/Parent# */

object XX {
  new Child().foo
  new Child().bar(1)
}/* internalOutgoingReferences(scalaclean/test/references/internalOutgoingReferences/XX.) - scalaclean/test/references/internalOutgoingReferences/Child# :: scalaclean/test/references/internalOutgoingReferences/Child#bar(). :: scalaclean/test/references/internalOutgoingReferences/Parent#foo(). */
