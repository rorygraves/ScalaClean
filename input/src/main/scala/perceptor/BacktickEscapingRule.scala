// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "backticks"
    pattern = "`.*`"
    message = "Backtick escaping"
  }
]
*/
// scalafix:on
package perceptor

class BacktickEscapingRule {

  val `type` = 2 // assert: DisableSyntax.backticks

  // TODO extend regex to allow ticks in comments ([^//] doesn't appear to work)
}
