package methodannotation

import scala.annotation.Annotation

object AnnotApp1 extends App {
  myMethod(3)

  @UsedAnnotation1(classOf[UsedByAnnotation1])
  @UsedJavaAnnotation1(clazz = classOf[UsedByAnnotation2])
  @UsedJavaAnnotation2(clazz = Array(classOf[UsedByAnnotation3]))
  //@UsedJavaAnnotation3(UsedJavaAnnotation1(classOf[UsedByAnnotation4]))
  def myMethod(@UsedMethodAnnotation x: Int) = {

  }
  @UsedAnnotation1(classOf[UsedByAnnotation1])
  @UsedJavaAnnotation1(clazz = classOf[UsedByAnnotation2])
  @UsedJavaAnnotation2(clazz = Array(classOf[UsedByAnnotation3]))
  //@UsedJavaAnnotation3(UsedJavaAnnotation1(classOf[UsedByAnnotation4]))
  def unused(@UsedMethodAnnotation x: Int) = {

  }
}

class UsedAnnotation1(value: Class[_]) extends Annotation

class UsedByAnnotation1(value: Class[_]) extends Annotation
class UsedByAnnotation2(value: Class[_]) extends Annotation
class UsedByAnnotation3(value: Class[_]) extends Annotation
class UsedByAnnotation4(value: Class[_]) extends Annotation

class UnusedAnnotation(value: Class[_]) extends Annotation

class UsedMethodAnnotation extends Annotation