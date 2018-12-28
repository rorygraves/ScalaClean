// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "now"
    pattern = "Instant.now"
    message = "Don't use Instant.now"
  }
]
*/
// scalafix:on
package perceptor

import java.time.Instant

class NowUsage {

  // need to add other nows to conf and example
  def now: Instant = {
    Instant.now // assert: DisableSyntax.now
  }
}