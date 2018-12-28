/*
rule = DisableSyntax

DisableSyntax.noReturns = true
*/
package perceptor

class NoReturnRule {

  def withReturnFails: Unit = {
   return () // assert: DisableSyntax.return
  }

  def withImplicitReturnPasses: Unit = () // scalafix: ok

}
