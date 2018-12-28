// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "javaconversions"
    pattern = "scala.collection.JavaConversions"
    message = "Java conversions is prohibited"
  }
]
*/
// scalafix:on
package perceptor

import scala.collection.JavaConversions // assert: DisableSyntax.javaconversions

class JavaConversionsRule {

  import scala.collection.JavaConversions // assert: DisableSyntax.javaconversions

}
