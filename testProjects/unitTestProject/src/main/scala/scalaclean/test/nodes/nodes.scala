package scalaclean.test.nodes

trait GrandParentTrait {
  def d1: Int
  def d2(a: Int): Int
  def d2a(a: Int)(b: Int): Int
  def d3()(): Int
  val x1: Int = 1
  val x2: Int
  var y1: Int = 1
  var y2: Int = _
  var y3: Int
  var y4: Int
}

trait ParentTrait extends GrandParentTrait {
  def d3(): () => Int = ???
}

trait ChildTrait extends ParentTrait {
  override def d3(): () => Int = ???
  def d2a(a: Int)              = d2 _

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

  override val x2: Int = 10
  override var y3: Int = 10
  override var y4: Int = _

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

  override val x2: Int = 10
  override var y3: Int = 10
  override var y4: Int = _
}

abstract class GrandParentClass {

  def foo: Unit = ()
  val x1: Int   = 1
  val x2: Int
  var y1: Int = 1
  var y2: Int = _
  var y3: Int

}

class ParentClass extends GrandParentClass {
  override val x2: Int = 9
  override var y3: Int = 9
}

class ChildClass extends ParentClass {
  def bar(x: Any): Unit = ()
}

object ChildObject extends ChildClass {
  new ChildClass().foo
  new ChildClass().bar(1)
}

object TestVarVal {
  val x1: Int      = 5
  var x2: Int      = 5
  lazy val x3: Int = 5

  val (x10, x11)      = (1, 2)
  var (x20, x21)      = (1, 2)
  lazy val (x30, x31) = (1, 2)

}
