package scalaclean.test.overrides.internalTransitiveOverrides

trait GrandParentTrait {
  def d1: Int
  def d2(a:Int): Int
  def d2a(a:Int)(b:Int): Int
  def d3()(): Int

}
trait ParentTrait extends GrandParentTrait {
  def d1: Int
  def d2(a: Int): Int = ???
  def d3(): () => Int = ???
}
trait ChildTrait extends ParentTrait {
  def d1: Int = 1
  override def d2(a: Int): Int = ???
  override def d3(): () => Int = ???
  def d2a(a:Int) = d2 _

}
trait GChildTrait extends ChildTrait {
  override def d1: Int = 1
  override def d2(a: Int): Int = ???
  override def d3(): () => Int = () => 1

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

trait T1 {
  def v1 : AnyRef
}

trait T2 extends T1 {
  def v1 : String
}
trait T3 extends T1 {
  def v1 : String = ""
}
trait T4 extends T2 {
  def v1 : String
}

trait VT2 extends T1 {
  val v1 : String
}
trait VT3 extends T1 {
  val v1 : String = ""
}
trait VT4 extends VT2 {
  val v1 : String
}

trait VL2 extends T1 {
  val v1 : String
}
trait VL3 extends VL2 {
  lazy val v1 : String = ""
}

abstract class VLC2 extends T1 {
  val v1 : String
}
class VLC3 extends VLC2 {
  lazy val v1 : String = ""
}
class VLC3a extends VLC3 {
  override lazy val v1 : String = ""
}
