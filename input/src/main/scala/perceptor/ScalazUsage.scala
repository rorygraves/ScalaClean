// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "scalaz"
    pattern = "scalaz"
    message = "scalaz is prohibited"
  }
]
*/
// scalafix:on
package perceptor

import scalaz.Disjunction // assert: DisableSyntax.scalaz

class ScalazUsage {

  def useScalaz(): Unit = ()
}
