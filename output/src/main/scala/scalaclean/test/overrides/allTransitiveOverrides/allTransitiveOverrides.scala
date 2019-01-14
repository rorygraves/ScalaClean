package scalaclean.test.overrides.allTransitiveOverrides

trait GrandParentTrait {
  def d1: Int
  def d2(a:Int): Int
  def d2a(a:Int)(b:Int): Int
  def d3()(): Int

}
trait ParentTrait extends GrandParentTrait {
  def d1: Int/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d1().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d1(). */
  def d2(a: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d2().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2(). */
  def d3(): () => Int = ???
}
trait ChildTrait extends ParentTrait {
  def d1: Int = 1/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/ChildTrait#d1().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d1(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d1(). */
  override def d2(a: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/ChildTrait#d2().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d2(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2(). */
  override def d3(): () => Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/ChildTrait#d3().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d3(). */
  def d2a(a:Int) = d2 _

}
trait GChildTrait extends ChildTrait {
  override def d1: Int = 1/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/GChildTrait#d1().) - scalaclean/test/overrides/allTransitiveOverrides/ChildTrait#d1(). :: scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d1(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d1(). */
  override def d2(a: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/GChildTrait#d2().) - scalaclean/test/overrides/allTransitiveOverrides/ChildTrait#d2(). :: scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d2(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2(). */
  override def d3(): () => Int = () => 1/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/GChildTrait#d3().) - scalaclean/test/overrides/allTransitiveOverrides/ChildTrait#d3(). :: scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d3(). */

}
trait Trait_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#equals().) - java/lang/Object#equals(). :: scala/Any#equals(). */

  override def clone(): AnyRef = super.clone()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#clone().) - java/lang/Object#clone(). */

  override def toString: String = super.toString/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#toString().) - java/lang/Object#toString(). :: scala/Any#toString(). */

  override def finalize(): Unit = super.finalize()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#finalize().) - java/lang/Object#finalize(). */

  override def hashCode(): Int = super.hashCode()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#hashCode().) - java/lang/Object#hashCode(). :: scala/Any#hashCode(). */

  override def d1: Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#d1().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d1(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d1(). */

  override def d2(a: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#d2().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d2(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2(). */

  override def d2a(a: Int)(b: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#d2a().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2a(). */

  override def d3()(): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Trait_ChildTrait#d3().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d3(). */
}
class Class_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#equals().) - java/lang/Object#equals(). :: scala/Any#equals(). */

  override def clone(): AnyRef = super.clone()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#clone().) - java/lang/Object#clone(). */

  override def toString: String = super.toString/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#toString().) - java/lang/Object#toString(). :: scala/Any#toString(). */

  override def finalize(): Unit = super.finalize()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#finalize().) - java/lang/Object#finalize(). */

  override def hashCode(): Int = super.hashCode()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#hashCode().) - java/lang/Object#hashCode(). :: scala/Any#hashCode(). */

  override def d1: Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#d1().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d1(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d1(). */

  override def d2(a: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#d2().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d2(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2(). */

  override def d2a(a: Int)(b: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#d2a().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2a(). */

  override def d3()(): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Class_ChildTrait#d3().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d3(). */
}
object Object_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.equals().) - java/lang/Object#equals(). :: scala/Any#equals(). */

  override def clone(): AnyRef = super.clone()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.clone().) - java/lang/Object#clone(). */

  override def toString: String = super.toString/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.toString().) - java/lang/Object#toString(). :: scala/Any#toString(). */

  override def finalize(): Unit = super.finalize()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.finalize().) - java/lang/Object#finalize(). */

  override def hashCode(): Int = super.hashCode()/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.hashCode().) - java/lang/Object#hashCode(). :: scala/Any#hashCode(). */

  override def d1: Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.d1().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d1(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d1(). */

  override def d2(a: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.d2().) - scalaclean/test/overrides/allTransitiveOverrides/ParentTrait#d2(). :: scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2(). */

  override def d2a(a: Int)(b: Int): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.d2a().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d2a(). */

  override def d3()(): Int = ???/* allTransitiveOverrides(scalaclean/test/overrides/allTransitiveOverrides/Object_ChildTrait.d3().) - scalaclean/test/overrides/allTransitiveOverrides/GrandParentTrait#d3(). */
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