// scalafix:off
/*
rule = DisableSyntax

DisableSyntax.regex = [
  {
    id = "ignore"
    pattern = "@Ignore\\s*[^(]"
    message = "Must be in format: '@Ignore(<jira url>)'"
  }
]
*/

package perceptor

case class Ignore(v: String = "") extends scala.annotation.StaticAnnotation


// TODO: Add url into regex between braces
class IgnoreRule {
  // scalafix:on

  @Ignore // assert: DisableSyntax.ignore
  def failOnVanillaIgnore = ()

  @Ignore("http://someurl.com/abcd") // scalafix: ok
  def allowWithUrl = ()

  @SuppressWarnings(Array("DisableSyntax.ignore"))
  @Ignore
  def passWhenSuppressedWithAnnotation = ()

}
