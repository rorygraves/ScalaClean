// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "banned-apis"
    pattern = "(KEYWORD.persist|KEYWORD.unsafe|KEYWORD.upsert)"
    message = "Don't use banned APIs"
  }
]
*/
// scalafix:on
package perceptor

object KEYWORD {
  def persist: Unit = ???
  def unsafe: Unit = ???
  def upsert: Unit = ???
}

class BannedAPIs {

  def useBannedAPIs(): Unit = {
    val thisIsOK = KEYWORD
    KEYWORD.persist // assert: DisableSyntax.banned-apis
    KEYWORD.unsafe // assert: DisableSyntax.banned-apis
    KEYWORD.upsert // assert: DisableSyntax.banned-apis
  }
}