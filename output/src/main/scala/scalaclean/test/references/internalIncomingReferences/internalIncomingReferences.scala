package scalaclean.test.references.internalIncomingReferences

class Parent {
  def foo: Unit = ()/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Parent#foo().) - scalaclean/test/references/internalIncomingReferences/XX. */
}/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Parent#) - scalaclean/test/references/internalIncomingReferences/Child# */
class Child extends Parent{
  def bar(x:Any): Unit = ()/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Child#bar().) - scalaclean/test/references/internalIncomingReferences/XX. */
}/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Child#) - scalaclean/test/references/internalIncomingReferences/XX. */

object XX {
  new Child().foo
  new Child().bar(1)
}
         