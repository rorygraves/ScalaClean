/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "methods"
    pattern = "def [A-Z]"
    message = "Use camelCase"
  }
]
*/
package perceptor

class MethodNames {

  def BadMethodName(): Unit = () // assert: DisableSyntax.methods
}