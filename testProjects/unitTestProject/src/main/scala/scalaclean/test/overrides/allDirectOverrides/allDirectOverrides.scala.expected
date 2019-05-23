package scalaclean.test.overrides.allDirectOverrides

trait GrandParentTrait {
  def d1: Int
  def d2(a:Int): Int
  def d2a(a:Int)(b:Int): Int
  def d3()(): Int

}
trait ParentTrait extends GrandParentTrait {
  def d1: Int/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/ParentTrait#d1().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d1(). */
  def d2(a: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/ParentTrait#d2().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d2(). */
  def d3(): () => Int = ???
}
trait ChildTrait extends ParentTrait {
  def d1: Int = 1/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/ChildTrait#d1().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d1(). */
  override def d2(a: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/ChildTrait#d2().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d2(). */
  override def d3(): () => Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/ChildTrait#d3().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d3(). */
  def d2a(a:Int) = d2 _

}
trait GChildTrait extends ChildTrait {
  override def d1: Int = 1/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/GChildTrait#d1().) - scalaclean/test/overrides/allDirectOverrides/ChildTrait#d1(). */
  override def d2(a: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/GChildTrait#d2().) - scalaclean/test/overrides/allDirectOverrides/ChildTrait#d2(). */
  override def d3(): () => Int = () => 1/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/GChildTrait#d3().) - scalaclean/test/overrides/allDirectOverrides/ChildTrait#d3(). */

}
trait Trait_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#equals().) - java/lang/Object#equals(). */

  override def clone(): AnyRef = super.clone()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#clone().) - java/lang/Object#clone(). */

  override def toString: String = super.toString/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#toString().) - java/lang/Object#toString(). */

  override def finalize(): Unit = super.finalize()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#finalize().) - java/lang/Object#finalize(). */

  override def hashCode(): Int = super.hashCode()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#hashCode().) - java/lang/Object#hashCode(). */

  override def d1: Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#d1().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d1(). */

  override def d2(a: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#d2().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d2(). */

  override def d2a(a: Int)(b: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#d2a().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d2a(). */

  override def d3()(): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Trait_ChildTrait#d3().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d3(). */
}
class Class_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#equals().) - java/lang/Object#equals(). */

  override def clone(): AnyRef = super.clone()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#clone().) - java/lang/Object#clone(). */

  override def toString: String = super.toString/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#toString().) - java/lang/Object#toString(). */

  override def finalize(): Unit = super.finalize()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#finalize().) - java/lang/Object#finalize(). */

  override def hashCode(): Int = super.hashCode()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#hashCode().) - java/lang/Object#hashCode(). */

  override def d1: Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#d1().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d1(). */

  override def d2(a: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#d2().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d2(). */

  override def d2a(a: Int)(b: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#d2a().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d2a(). */

  override def d3()(): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Class_ChildTrait#d3().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d3(). */
}
object Object_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.equals().) - java/lang/Object#equals(). */

  override def clone(): AnyRef = super.clone()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.clone().) - java/lang/Object#clone(). */

  override def toString: String = super.toString/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.toString().) - java/lang/Object#toString(). */

  override def finalize(): Unit = super.finalize()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.finalize().) - java/lang/Object#finalize(). */

  override def hashCode(): Int = super.hashCode()/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.hashCode().) - java/lang/Object#hashCode(). */

  override def d1: Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.d1().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d1(). */

  override def d2(a: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.d2().) - scalaclean/test/overrides/allDirectOverrides/ParentTrait#d2(). */

  override def d2a(a: Int)(b: Int): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.d2a().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d2a(). */

  override def d3()(): Int = ???/* allDirectOverrides(scalaclean/test/overrides/allDirectOverrides/Object_ChildTrait.d3().) - scalaclean/test/overrides/allDirectOverrides/GrandParentTrait#d3(). */
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