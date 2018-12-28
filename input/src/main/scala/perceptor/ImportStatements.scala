/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "import-statements"
    pattern = "import o.p.[a-zA-Z]+"
    message = "Use import o.p._"
  }
]
*/
package perceptor

package o {
  class Dummy
  package p {
    class OtherDummy
  }
}

class ImportStatements {
  import o.p.OtherDummy // assert: DisableSyntax.import-statements
}