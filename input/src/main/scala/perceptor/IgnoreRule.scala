// scalafix:off
/*
rule = DisableSyntax

DisableSyntax.regex = [
  {
    id = "inore"
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

  @Ignore // assert: DisableSyntax.inore
  def failOnVanillaIgnore = ()

  @Ignore("http://someurl.com/abcd") // scalafix: ok
  def allowWithUrl = ()

  @SuppressWarnings(Array("DisableSyntax.inore"))
  @Ignore
  def passWhenSuppressedWithAnnotation = ()

}
