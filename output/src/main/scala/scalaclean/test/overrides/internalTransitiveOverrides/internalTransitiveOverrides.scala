package scalaclean.test.overrides.internalTransitiveOverrides

trait GrandParentTrait {
  def d1: Int
  def d2(a:Int): Int
  def d2a(a:Int)(b:Int): Int
  def d3()(): Int

}
trait ParentTrait extends GrandParentTrait {
  def d1: Int/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d1().) - MethodModel d1 [7:2 - 7:13] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d1().) */
  def d2(a: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d2().) - MethodModel d2 [8:2 - 8:20] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2().) */
  def d3(): () => Int = ???
}
trait ChildTrait extends ParentTrait {
  def d1: Int = 1/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/ChildTrait#d1().) - MethodModel d1 [7:2 - 7:13] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d1().) :: MethodModel d1 [14:2 - 14:13] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d1().) */
  override def d2(a: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/ChildTrait#d2().) - MethodModel d2 [8:2 - 8:20] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2().) :: MethodModel d2 [15:2 - 15:27] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d2().) */
  override def d3(): () => Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/ChildTrait#d3().) - MethodModel d3 [16:2 - 16:27] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d3().) */
  def d2a(a:Int) = d2 _

}
trait GChildTrait extends ChildTrait {
  override def d1: Int = 1/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/GChildTrait#d1().) - MethodModel d1 [19:2 - 19:17] (scalaclean/test/overrides/internalTransitiveOverrides/ChildTrait#d1().) :: MethodModel d1 [7:2 - 7:13] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d1().) :: MethodModel d1 [14:2 - 14:13] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d1().) */
  override def d2(a: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/GChildTrait#d2().) - MethodModel d2 [20:2 - 20:36] (scalaclean/test/overrides/internalTransitiveOverrides/ChildTrait#d2().) :: MethodModel d2 [8:2 - 8:20] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2().) :: MethodModel d2 [15:2 - 15:27] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d2().) */
  override def d3(): () => Int = () => 1/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/GChildTrait#d3().) - MethodModel d3 [21:2 - 21:36] (scalaclean/test/overrides/internalTransitiveOverrides/ChildTrait#d3().) :: MethodModel d3 [16:2 - 16:27] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d3().) */

}
trait Trait_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def clone(): AnyRef = super.clone()

  override def toString: String = super.toString

  override def finalize(): Unit = super.finalize()

  override def hashCode(): Int = super.hashCode()

  override def d1: Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Trait_ChildTrait#d1().) - MethodModel d1 [7:2 - 7:13] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d1().) :: MethodModel d1 [14:2 - 14:13] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d1().) */

  override def d2(a: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Trait_ChildTrait#d2().) - MethodModel d2 [8:2 - 8:20] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2().) :: MethodModel d2 [15:2 - 15:27] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d2().) */

  override def d2a(a: Int)(b: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Trait_ChildTrait#d2a().) - MethodModel d2a [9:2 - 9:28] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2a().) */

  override def d3()(): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Trait_ChildTrait#d3().) - MethodModel d3 [10:2 - 10:17] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d3().) */
}
class Class_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def clone(): AnyRef = super.clone()

  override def toString: String = super.toString

  override def finalize(): Unit = super.finalize()

  override def hashCode(): Int = super.hashCode()

  override def d1: Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Class_ChildTrait#d1().) - MethodModel d1 [7:2 - 7:13] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d1().) :: MethodModel d1 [14:2 - 14:13] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d1().) */

  override def d2(a: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Class_ChildTrait#d2().) - MethodModel d2 [8:2 - 8:20] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2().) :: MethodModel d2 [15:2 - 15:27] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d2().) */

  override def d2a(a: Int)(b: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Class_ChildTrait#d2a().) - MethodModel d2a [9:2 - 9:28] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2a().) */

  override def d3()(): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Class_ChildTrait#d3().) - MethodModel d3 [10:2 - 10:17] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d3().) */
}
object Object_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def clone(): AnyRef = super.clone()

  override def toString: String = super.toString

  override def finalize(): Unit = super.finalize()

  override def hashCode(): Int = super.hashCode()

  override def d1: Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Object_ChildTrait.d1().) - MethodModel d1 [7:2 - 7:13] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d1().) :: MethodModel d1 [14:2 - 14:13] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d1().) */

  override def d2(a: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Object_ChildTrait.d2().) - MethodModel d2 [8:2 - 8:20] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2().) :: MethodModel d2 [15:2 - 15:27] (scalaclean/test/overrides/internalTransitiveOverrides/ParentTrait#d2().) */

  override def d2a(a: Int)(b: Int): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Object_ChildTrait.d2a().) - MethodModel d2a [9:2 - 9:28] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d2a().) */

  override def d3()(): Int = ???/* internalTransitiveOverrides(scalaclean/test/overrides/internalTransitiveOverrides/Object_ChildTrait.d3().) - MethodModel d3 [10:2 - 10:17] (scalaclean/test/overrides/internalTransitiveOverrides/GrandParentTrait#d3().) */
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
