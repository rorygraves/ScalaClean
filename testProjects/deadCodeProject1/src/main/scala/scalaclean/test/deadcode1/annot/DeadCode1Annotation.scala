package scalaclean.test.deadcode1.annot

object AnnotApp1 extends App {}

case class UnusedClass(value: String)

@deprecated
case class UnusedClassWithAnnotation(value: String)

/** some docs */
object UnusedObjectDoc
