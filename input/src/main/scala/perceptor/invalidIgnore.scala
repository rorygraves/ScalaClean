// scalafix:off
/*
rule = DisableSyntax

DisableSyntax.regex = [
  {
    id = "inore"
    pattern = "@Ignore"
    message = "Must be in format: '@KEYWORD(<jira url>)"
  }
]
*/
// scalafix:on
package perceptor

import jdk.nashorn.internal.ir.annotations.Ignore


class invalidIgnore {

  @Ignore // assert: DisableSyntax.inore
  def xyz = ()

}
