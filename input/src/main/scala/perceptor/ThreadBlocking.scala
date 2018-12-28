// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "thread-blocking"
    pattern = "Thread.sleep"
    message = "Avoid blocking"
  }
]
*/
// scalafix:on
package perceptor

class ThreadBlocking {

  def sleep: Unit = {
    Thread.sleep(100) // assert: DisableSyntax.thread-blocking
  }
}