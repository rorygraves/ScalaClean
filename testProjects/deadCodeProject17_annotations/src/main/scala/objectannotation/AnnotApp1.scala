package objectannotation

import scala.annotation.Annotation

object AnnotApp1 extends App {
  UsedObject
}


@UsedAnnotation1(classOf[UsedByAnnotation1])
@UsedJavaAnnotation1(clazz = classOf[UsedByAnnotation2])
@UsedJavaAnnotation2(clazz = Array(classOf[UsedByAnnotation3]))
//@UsedJavaAnnotation3(UsedJavaAnnotation1(classOf[UsedByAnnotation4]))
object UsedObject {
  def unused1 = ???
}

class UsedAnnotation1(value: Class[_]) extends Annotation

class UsedByAnnotation1(value: Class[_]) extends Annotation
class UsedByAnnotation2(value: Class[_]) extends Annotation
class UsedByAnnotation3(value: Class[_]) extends Annotation
class UsedByAnnotation4(value: Class[_]) extends Annotation

class UnusedAnnotation(value: Class[_]) extends Annotation

class UsedMethodAnnotation extends Annotation