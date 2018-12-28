// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "currency-apis"
    pattern = "OldCurrencyUnit"
    message = "Use CurrencyUnit"
  }
]
*/
// scalafix:on
package perceptor

class CurrencyUnit
class OldCurrencyUnit // assert: DisableSyntax.currency-apis

class CurrencyApis {

  def useBannedAPIs(): Unit = {
    val thisIsOK = new CurrencyUnit
    val notOK = new OldCurrencyUnit // assert: DisableSyntax.currency-apis
  }
}