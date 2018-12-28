// scalafix:off
/*
rule = DisableSyntax

DisableSyntax.regex = [
  {
    id = "todo"
    pattern = "(TODO\\s*[^(| =]|FIXME)"
    message = "Must be in format TODO (<jira url>)"
  }
]
*/
package perceptor

// TODO: Add url into regex between braces
class ToDoRule {
  // scalafix:on

 def TODO() = () // scalafix: ok

 def aTODO = () // scalafix: ok

  "TODO(http://jira.com/abcd)" // scalafix: ok

  "TODO" // assert: DisableSyntax.todo

  "FIXME" // assert: DisableSyntax.todo

}
