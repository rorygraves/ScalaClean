/*
rules = [ ScalaCleanPrivatiserAnalysis , ScalaCleanPrivatiserApply ]

*/
package scalaclean.privatiser

import scala.collection.immutable

/*
What can be private:

Classes + inner
Objects + inner
Methods
Val/vars

 */
abstract class Parent[I] {
  def foo(i:I): Unit

  def v1: I
  def va: I
  def v2: I
  def v3: I
  def v4: AnyRef
}
class child extends Parent[Int] {
  def foo(i:Int) = ()

  def v1 = 0
  def va() = 0
  def va(i:Int) = 0
  val v2 = 1
  var v3 = 2
  object v4 extends child
}
class OuterClass {

  val iAmPrivateVal = 24
  val iAmPublicVal = iAmPrivateVal

  var iAmPrivateVar = 24
  var iAmPublicVar = iAmPrivateVar

  class InnerClassPrivate {
    def foo = shouldBePrivate

    def shouldBePrivate = ???
  }

  object InnerObjectPrivate {
    def baz = shouldBePrivate

    def shouldBePrivate = ???
  }

  class InnerClassPublic {
    def foo = shouldBePrivate

    def shouldBePrivate = ???
  }

  object InnerObjectPublic {
    def baz = shouldBePrivate

    def shouldBePrivate = ???
  }

  def shouldBePrivate = {
    new InnerClassPrivate().foo
    InnerObjectPrivate.baz
  }

  def calledExternally = shouldBePrivate
}


object TestMain extends App {
  // Call method on public nested object
  val outer = new OuterClass
  outer.InnerObjectPublic.baz

  // Call method on public nested class
  val innerClass = new outer.InnerClassPublic
  innerClass.foo

  // Call elements that need to be public
  outer.calledExternally
  outer.iAmPublicVar += 1
  outer.iAmPublicVal + outer.iAmPublicVar
}
