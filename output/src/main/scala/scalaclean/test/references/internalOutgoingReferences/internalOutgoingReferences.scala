package scalaclean.test.references.internalOutgoingReferences

class Parent {
  def foo: Unit = ()
}
class Child extends Parent{
  def bar(x:Any): Unit = ()
}/* internalOutgoingReferences(scalaclean/test/references/internalOutgoingReferences/Child#) - scalaclean/test/references/internalOutgoingReferences/Parent# */
object Special {
  def myVar1 = 1
  def myVar1_=(i: Int) = ()

  def myVar2 = 1
  def myVar2_=(i: Int) = ()

  def apply() = 7
  def unapply(a:Any) = Some(1,2)

  def update(i: Int, j: Int) = ()

}

object XX {
  new Child().foo
  new Child().bar(1)

  Special.myVar1 += 1

  Special.myVar2 = 9
  println(Special.myVar2)

  Special()
  val x: Any = 1
  x match {
    case i: Int => ???
    case Special(a,b) => ???
  }

  Special(1) = 7
}/* internalOutgoingReferences(scalaclean/test/references/internalOutgoingReferences/XX.) - scalaclean/test/references/internalOutgoingReferences/Child# :: scalaclean/test/references/internalOutgoingReferences/Child#bar(). :: scalaclean/test/references/internalOutgoingReferences/Parent#foo(). :: scalaclean/test/references/internalOutgoingReferences/Special. :: scalaclean/test/references/internalOutgoingReferences/Special.`myVar1_=`(). :: scalaclean/test/references/internalOutgoingReferences/Special.`myVar2_=`(). :: scalaclean/test/references/internalOutgoingReferences/Special.myVar2(). :: scalaclean/test/references/internalOutgoingReferences/Special.update(). :: scalaclean/test/references/internalOutgoingReferences/XX.x. */