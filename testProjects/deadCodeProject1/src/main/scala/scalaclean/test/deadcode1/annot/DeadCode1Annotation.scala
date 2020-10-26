package scalaclean.test.deadcode1.annot
import java.lang.annotation.Inherited

object AnnotApp1 extends App {}

case class UnusedClass(value: String)

@deprecated
case class UnusedClassWithAnnotation(value: String)

/** some docs */
object UnusedObjectDoc

@Inherited
@deprecated
case class UnusedClassWithAnnotation2(value: String)

/** some docs */
@Inherited
@deprecated
object UnusedObjectDoc2

//some comments
@Inherited
//some comments
@deprecated
//some comments
case class UnusedClassWithAnnotation3(value: String)

//some comments
/** some docs */
@Inherited
//some comments
@deprecated
//some comments
object UnusedObjectDoc3
