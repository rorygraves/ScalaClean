// scalafix:off
/*
rule = DisableSyntax

DisableSyntax.regex = [
  {
    id = "tdo"
    pattern = "(TODO|FIXME)"
    message = "Must be in format <KEYWORD>(<jiraurl)"
  }
]
 */
// scalafix:on
package perceptor

class invalidTO_DO {

  // Note: error here is reported but not working as intended yet inside of comments so keeping as code for now
 def TODO() = () // assert: DisableSyntax.tdo

}
