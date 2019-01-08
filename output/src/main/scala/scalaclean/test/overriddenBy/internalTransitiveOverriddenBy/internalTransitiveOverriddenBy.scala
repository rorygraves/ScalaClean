package scalaclean.test.overriddenBy.internalTransitiveOverriddenBy

trait GrandParentTrait {
  def d1: Int
  def d2(a:Int): Int
  def d2a(a:Int)(b:Int): Int
  def d3()(): Int

}
trait ParentTrait extends GrandParentTrait {
  def d3(): () => Int = ???
}
trait ChildTrait extends ParentTrait {
  override def d3(): () => Int = ???/* internalTransitiveOverriddenBy(scalaclean/test/overriddenBy/internalTransitiveOverriddenBy/ChildTrait#d3().) - def d3 [14:2 - 14:27]  */
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
