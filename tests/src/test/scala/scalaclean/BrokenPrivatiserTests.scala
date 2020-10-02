package scalaclean

import scalaclean.cli.SCOptions

// These tests are known to be broken - once they are working move them into PrivatiserTests
class BrokenPrivatiserTests extends AbstractProjectTests {
  val expectationSuffix: String = ""
  val taskName: String          = SCOptions.privatiserCmd

  test("privatiser2") {
    privatiserProjectTest("privatiserProject2")
  }

  test("privatiser4") {
    privatiserProjectTest("privatiserProject4")
  }

  test("privatiser5") {
    privatiserProjectTest("privatiserProject5")
  }
}
