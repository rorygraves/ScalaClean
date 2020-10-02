package scalaclean

import org.junit.Test
import scalaclean.cli.SCOptions

class FinaliserTests extends AbstractProjectTests {
  val expectationSuffix: String = ""
  val taskName: String          = SCOptions.finaliserCmd

  @Test def finaliser1(): Unit = {
    finaliserProjectTest("finaliserProject1")
  }

}
