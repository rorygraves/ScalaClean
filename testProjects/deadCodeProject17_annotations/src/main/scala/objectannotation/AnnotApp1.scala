package objectannotation

object AnnotApp1 extends App {
  UsedObject

  myMethod(3)

  def myMethod(@UsedMethodAnnotation x: Int) = {

  }
}


@UsedAnnotation1(classOf[UsedByAnnotation1])
object UsedObject {
  def unused1 = ???
}

class UsedAnnotation1(value: Class[_])

class UsedByAnnotation1(value: Class[_])

class UnusedAnnotation(value: Class[_])

class UsedMethodAnnotation
