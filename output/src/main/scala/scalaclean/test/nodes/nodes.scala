
package scalaclean.test.nodes

trait GrandParentTrait {
  def d1: Int
  def d2(a:Int): Int
  def d2a(a:Int)(b:Int): Int
  def d3()(): Int

}/* TraitModel GrandParentTrait [6:0 - 12:1]  */
trait ParentTrait extends GrandParentTrait {
  def d3(): () => Int = ???/* MethodModel d3 [14:2 - 14:27]  */
}/* TraitModel ParentTrait [13:0 - 15:1]  */
trait ChildTrait extends ParentTrait {
  override def d3(): () => Int = ???/* MethodModel d3 [17:2 - 17:36]  */
  def d2a(a:Int) = d2 _/* MethodModel d2a [18:2 - 18:23]  */

}/* TraitModel ChildTrait [16:0 - 20:1]  */
trait Trait_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* MethodModel equals [22:2 - 22:60]  */

  override def clone(): AnyRef = super.clone()/* MethodModel clone [24:2 - 24:46]  */

  override def toString: String = super.toString/* MethodModel toString [26:2 - 26:48]  */

  override def finalize(): Unit = super.finalize()/* MethodModel finalize [28:2 - 28:50]  */

  override def hashCode(): Int = super.hashCode()/* MethodModel hashCode [30:2 - 30:49]  */

  override def d1: Int = ???/* MethodModel d1 [32:2 - 32:28]  */

  override def d2(a: Int): Int = ???/* MethodModel d2 [34:2 - 34:36]  */

  override def d2a(a: Int)(b: Int): Int = ???/* MethodModel d2a [36:2 - 36:45]  */

  override def d3()(): Int = ???/* MethodModel d3 [38:2 - 38:32]  */
}/* TraitModel Trait_ChildTrait [21:0 - 39:1]  */
class Class_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* MethodModel equals [41:2 - 41:60]  */

  override def clone(): AnyRef = super.clone()/* MethodModel clone [43:2 - 43:46]  */

  override def toString: String = super.toString/* MethodModel toString [45:2 - 45:48]  */

  override def finalize(): Unit = super.finalize()/* MethodModel finalize [47:2 - 47:50]  */

  override def hashCode(): Int = super.hashCode()/* MethodModel hashCode [49:2 - 49:49]  */

  override def d1: Int = ???/* MethodModel d1 [51:2 - 51:28]  */

  override def d2(a: Int): Int = ???/* MethodModel d2 [53:2 - 53:36]  */

  override def d2a(a: Int)(b: Int): Int = ???/* MethodModel d2a [55:2 - 55:45]  */

  override def d3()(): Int = ???/* MethodModel d3 [57:2 - 57:32]  */
}/* ClassModel Class_ChildTrait [40:0 - 58:1]  */
object Object_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* MethodModel equals [60:2 - 60:60]  */

  override def clone(): AnyRef = super.clone()/* MethodModel clone [62:2 - 62:46]  */

  override def toString: String = super.toString/* MethodModel toString [64:2 - 64:48]  */

  override def finalize(): Unit = super.finalize()/* MethodModel finalize [66:2 - 66:50]  */

  override def hashCode(): Int = super.hashCode()/* MethodModel hashCode [68:2 - 68:49]  */

  override def d1: Int = ???/* MethodModel d1 [70:2 - 70:28]  */

  override def d2(a: Int): Int = ???/* MethodModel d2 [72:2 - 72:36]  */

  override def d2a(a: Int)(b: Int): Int = ???/* MethodModel d2a [74:2 - 74:45]  */

  override def d3()(): Int = ???/* MethodModel d3 [76:2 - 76:32]  */
}/* ObjectModel Object_ChildTrait [59:0 - 77:1]  */


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