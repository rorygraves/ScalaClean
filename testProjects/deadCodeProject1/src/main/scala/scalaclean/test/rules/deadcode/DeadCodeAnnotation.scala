package scalaclean.test.rules.deadcode.deadannotation

object App1 extends App {}
case class UnusedClass(value: String)

@deprecated
case class UnusedClassWithAnnotation(value: String)
/** some docs */
object UnusedObjectDoc