package scalaclean.privatiser

/*
What can be private:

Classes + inner
Objects + inner
Methods
Val/vars

 */

class OuterClass {

  private val iAmPrivateVal = 24
  val iAmPublicVal = iAmPrivateVal

  private var iAmPrivateVar = 24
  var iAmPublicVar = iAmPrivateVar

  private class InnerClassPrivate {
    def foo = shouldBePrivate

    private def shouldBePrivate = ???
  }

  private object InnerObjectPrivate {
    def baz = shouldBePrivate

    private def shouldBePrivate = ???
  }

  class InnerClassPublic {
    def foo = shouldBePrivate

    private def shouldBePrivate = ???
  }

  object InnerObjectPublic {
    def baz = shouldBePrivate

    private def shouldBePrivate = ???
  }

  private def shouldBePrivate = {
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
