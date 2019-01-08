
package scalaclean.test.references.allOutgoingReferences

class Parent {
  def foo: Unit = ()/* allOutgoingReferences(scalaclean/test/references/allOutgoingReferences/Parent#foo().) - scala/Unit# */
}/* allOutgoingReferences(scalaclean/test/references/allOutgoingReferences/Parent#) - scalaclean/test/references/allOutgoingReferences/Parent#`<init>`(). */
class Child extends Parent{
  def bar(x:Any): Unit = ()/* allOutgoingReferences(scalaclean/test/references/allOutgoingReferences/Child#bar().) - scala/Any# :: scala/Unit# :: scalaclean/test/references/allOutgoingReferences/Child#bar().(x) */
}/* allOutgoingReferences(scalaclean/test/references/allOutgoingReferences/Child#) - scalaclean/test/references/allOutgoingReferences/Child#`<init>`(). :: scalaclean/test/references/allOutgoingReferences/Parent# :: scalaclean/test/references/allOutgoingReferences/Parent#`<init>`(). */

object XX {
  new Child().foo
  new Child().bar(1)
}/* allOutgoingReferences(scalaclean/test/references/allOutgoingReferences/XX.) - scalaclean/test/references/allOutgoingReferences/Child# :: scalaclean/test/references/allOutgoingReferences/Child#`<init>`(). :: scalaclean/test/references/allOutgoingReferences/Child#bar(). :: scalaclean/test/references/allOutgoingReferences/Parent#foo(). */
