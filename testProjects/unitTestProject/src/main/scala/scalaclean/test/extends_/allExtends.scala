package scalaclean.test.extends_

trait GrandParentTrait extends Iterable[String]

trait ParentTrait extends GrandParentTrait with java.util.Iterator[String]

trait ChildTrait extends ParentTrait

trait GChildTrait extends ChildTrait

trait Trait_ChildTrait extends ParentTrait

abstract class Class_ChildTrait extends ParentTrait

object Object_ChildTrait extends ParentTrait {
  override def iterator: Iterator[String] = ???

  override def hasNext: Boolean = ???

  override def next(): String = ???
}

trait Mix1

trait Mix2

trait Mix3

class GrandParentClass extends Iterable[String] {
  self: Mix2 =>
  override def iterator: Iterator[String] = ???
}

abstract class ParentClass extends GrandParentClass with Mix2

class ChildClass extends ParentClass
class Marker2 extends ParentClass

object ChildObject extends ChildClass
object Marker1 extends ChildClass