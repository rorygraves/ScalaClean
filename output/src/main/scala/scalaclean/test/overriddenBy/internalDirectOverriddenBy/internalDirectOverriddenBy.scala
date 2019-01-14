package scalaclean.test.overriddenBy.internalDirectOverriddenBy

trait GrandParentTrait {
  def d1: Int/* internalDirectOverriddenBy(scalaclean/test/overriddenBy/internalDirectOverriddenBy/GrandParentTrait#d1().) - MethodModel d1 [70:2 - 70:28] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Object_ChildTrait.d1().) :: MethodModel d1 [51:2 - 51:28] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Class_ChildTrait#d1().) :: MethodModel d1 [32:2 - 32:28] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Trait_ChildTrait#d1().) */
  def d2(a:Int): Int/* internalDirectOverriddenBy(scalaclean/test/overriddenBy/internalDirectOverriddenBy/GrandParentTrait#d2().) - MethodModel d2 [72:2 - 72:36] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Object_ChildTrait.d2().) :: MethodModel d2 [53:2 - 53:36] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Class_ChildTrait#d2().) :: MethodModel d2 [34:2 - 34:36] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Trait_ChildTrait#d2().) */
  def d2a(a:Int)(b:Int): Int/* internalDirectOverriddenBy(scalaclean/test/overriddenBy/internalDirectOverriddenBy/GrandParentTrait#d2a().) - MethodModel d2a [74:2 - 74:45] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Object_ChildTrait.d2a().) :: MethodModel d2a [55:2 - 55:45] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Class_ChildTrait#d2a().) :: MethodModel d2a [36:2 - 36:45] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Trait_ChildTrait#d2a().) */
  def d3()(): Int/* internalDirectOverriddenBy(scalaclean/test/overriddenBy/internalDirectOverriddenBy/GrandParentTrait#d3().) - MethodModel d3 [76:2 - 76:32] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Object_ChildTrait.d3().) :: MethodModel d3 [57:2 - 57:32] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Class_ChildTrait#d3().) :: MethodModel d3 [38:2 - 38:32] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/Trait_ChildTrait#d3().) */

}
trait ParentTrait extends GrandParentTrait {
  def d3(): () => Int = ???/* internalDirectOverriddenBy(scalaclean/test/overriddenBy/internalDirectOverriddenBy/ParentTrait#d3().) - MethodModel d3 [17:2 - 17:36] (scalaclean/test/overriddenBy/internalDirectOverriddenBy/ChildTrait#d3().) */
}
trait ChildTrait extends ParentTrait {
  override def d3(): () => Int = ???
  def d2a(a:Int) = d2 _

}
trait Trait_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def clone(): AnyRef = super.clone()

  override def toString: String = super.toString

  override def finalize(): Unit = super.finalize()

  override def hashCode(): Int = super.hashCode()

  override def d1: Int = ???

  override def d2(a: Int): Int = ???

  override def d2a(a: Int)(b: Int): Int = ???

  override def d3()(): Int = ???
}
class Class_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def clone(): AnyRef = super.clone()

  override def toString: String = super.toString

  override def finalize(): Unit = super.finalize()

  override def hashCode(): Int = super.hashCode()

  override def d1: Int = ???

  override def d2(a: Int): Int = ???

  override def d2a(a: Int)(b: Int): Int = ???

  override def d3()(): Int = ???
}
object Object_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def clone(): AnyRef = super.clone()

  override def toString: String = super.toString

  override def finalize(): Unit = super.finalize()

  override def hashCode(): Int = super.hashCode()

  override def d1: Int = ???

  override def d2(a: Int): Int = ???

  override def d2a(a: Int)(b: Int): Int = ???

  override def d3()(): Int = ???
}


//class GrandParentClass {
//
//  def foo: Unit = ()
//}
//class ParentClass extends GrandParentClass {
//}
//class ChildClass extends ParentClass{
//  def bar(x:Any): Unit = ()
//}
//
//object ClildObject extends ChildClass {
//  new Child().foo
//  new Child().bar(1)
//}