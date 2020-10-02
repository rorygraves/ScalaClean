package scalaclean

import scalaclean.cli.SCOptions

class PrivatiserTests extends AbstractProjectTests {
  val expectationSuffix: String = ""
  val taskName: String          = SCOptions.privatiserCmd

  test("privatiser1") {
    privatiserProjectTest("privatiserProject1")
  }

  test("privatiser3") {
    privatiserProjectTest("privatiserProject3")
  }

  test("privatiser6-annotations") {
    privatiserProjectTest("privatiserProject6")
  }

  test("privatiser7") {
    privatiserProjectTest("privatiserProject7")
  }

}
