package scalaclean.test.annotation

class BasicScala extends scala.annotation.StaticAnnotation {

}

class ComplexScala(
                    intValue: Int,
                    doubleValue: Double,
                    stringValue: String,
                    booleanValue: Boolean,
                    intValueWithDefault: Int = 42) extends scala.annotation.StaticAnnotation
