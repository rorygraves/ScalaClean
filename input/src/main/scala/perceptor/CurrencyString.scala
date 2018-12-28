// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "currency-string"
    pattern = "(ccy: String|ccy: Option\\[String\\]|currency: String|Ccy: String|Ccy: Option\\[String\\])"
    message = "Use CurrencyUnit"
  }
]
*/
// scalafix:on
package perceptor

class CurrencyString {

  def useBannedAPIs(): Unit = {
    val ccy: String = "USD" // assert: DisableSyntax.currency-string
    val optCcy: Option[String] = Some("USD") // assert: DisableSyntax.currency-string
  }
}