package scalaclean.test.nodes

trait GrandParentTrait {
  def d1: Int
  def d2(a:Int): Int
  def d2a(a:Int)(b:Int): Int
  def d3()(): Int

}/* trait GrandParentTrait [6:0 - 12:1]  */
trait ParentTrait extends GrandParentTrait {
  def d3(): () => Int = ???/* def d3 [14:2 - 14:27]  */
}/* trait ParentTrait [13:0 - 15:1]  */
trait ChildTrait extends ParentTrait {
  override def d3(): () => Int = ???/* def d3 [17:2 - 17:36]  */
  def d2a(a:Int) = d2 _/* def d2a [18:2 - 18:23]  */

}/* trait ChildTrait [16:0 - 20:1]  */
trait Trait_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* def equals [22:2 - 22:60]  */

  override def clone(): AnyRef = super.clone()/* def clone [24:2 - 24:46]  */

  override def toString: String = super.toString/* def toString [26:2 - 26:48]  */

  override def finalize(): Unit = super.finalize()/* def finalize [28:2 - 28:50]  */

  override def hashCode(): Int = super.hashCode()/* def hashCode [30:2 - 30:49]  */

  override def d1: Int = ???/* def d1 [32:2 - 32:28]  */

  override def d2(a: Int): Int = ???/* def d2 [34:2 - 34:36]  */

  override def d2a(a: Int)(b: Int): Int = ???/* def d2a [36:2 - 36:45]  */

  override def d3()(): Int = ???/* def d3 [38:2 - 38:32]  */
}/* trait Trait_ChildTrait [21:0 - 39:1]  */
class Class_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* def equals [41:2 - 41:60]  */

  override def clone(): AnyRef = super.clone()/* def clone [43:2 - 43:46]  */

  override def toString: String = super.toString/* def toString [45:2 - 45:48]  */

  override def finalize(): Unit = super.finalize()/* def finalize [47:2 - 47:50]  */

  override def hashCode(): Int = super.hashCode()/* def hashCode [49:2 - 49:49]  */

  override def d1: Int = ???/* def d1 [51:2 - 51:28]  */

  override def d2(a: Int): Int = ???/* def d2 [53:2 - 53:36]  */

  override def d2a(a: Int)(b: Int): Int = ???/* def d2a [55:2 - 55:45]  */

  override def d3()(): Int = ???/* def d3 [57:2 - 57:32]  */
}/* class Class_ChildTrait [40:0 - 58:1]  */
object Object_ChildTrait extends ParentTrait {
  override def equals(obj: Any): Boolean = super.equals(obj)/* def equals [60:2 - 60:60]  */

  override def clone(): AnyRef = super.clone()/* def clone [62:2 - 62:46]  */

  override def toString: String = super.toString/* def toString [64:2 - 64:48]  */

  override def finalize(): Unit = super.finalize()/* def finalize [66:2 - 66:50]  */

  override def hashCode(): Int = super.hashCode()/* def hashCode [68:2 - 68:49]  */

  override def d1: Int = ???/* def d1 [70:2 - 70:28]  */

  override def d2(a: Int): Int = ???/* def d2 [72:2 - 72:36]  */

  override def d2a(a: Int)(b: Int): Int = ???/* def d2a [74:2 - 74:45]  */

  override def d3()(): Int = ???/* def d3 [76:2 - 76:32]  */
}/* object Object_ChildTrait [59:0 - 77:1]  */


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
