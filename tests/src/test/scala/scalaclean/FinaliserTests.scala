package scalaclean

import scalaclean.cli.SCOptions

class FinaliserTests extends AbstractProjectTests {
  val expectationSuffix: String = ""
  val taskName: String          = SCOptions.finaliserCmd

  test("finaliser1") {
    finaliserProjectTest("finaliserProject1")
  }

}
