/*
rule = DisableSyntax

DisableSyntax.noReturns = true
*/
package perceptor

class noReturns {

  def abc: Unit = {
   return () // assert: DisableSyntax.return
  }

}
