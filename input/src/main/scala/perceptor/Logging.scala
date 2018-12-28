// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "logging"
    pattern = "(println|isDebugEnabled)"
    message = "Use a logger"
  }
]
*/
// scalafix:on
package perceptor

class Logging {

  def print(): Unit = {
    println("This is bad") // assert: DisableSyntax.logging
  }
}