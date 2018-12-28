// scalafix:off
/*
rule = DisableSyntax
DisableSyntax.regex = [
  {
    id = "async-wrappers"
    pattern = "(async[(]|aseq)"
    message = "Use collection.apar"
  }
]
*/
// scalafix:on
package perceptor

class AsyncWrappers {

  implicit class AsyncWrapper(collection: Seq[Int]) {
    def apar(): Unit = ()
    def aseq(): Unit = () // assert: DisableSyntax.async-wrappers
  }

  def async(coll: Seq[Int]) = ??? // assert: DisableSyntax.async-wrappers

  def useAsyncWrappers(): Unit = {
    val collection = Seq(1, 2, 3)
    async(collection) // assert: DisableSyntax.async-wrappers
    collection.apar
    collection.aseq // assert: DisableSyntax.async-wrappers
  }
}