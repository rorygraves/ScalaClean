package scalaclean.test.annotation

@BasicJava
@ComplexJava(intValue = 1, doubleValue = 2, stringValue = "XX", booleanValue = true)
@BasicScala
@ComplexScala(1, 2, "yy", true)
class annotated {
  @BasicJava
  @ComplexJava(intValue = 1, doubleValue = 2, stringValue = "XX", booleanValue = true, intValueWithDefault = 99)
  @BasicScala
  @ComplexScala(1, 2, "yy", true, 99)
  def foo = 0
}
