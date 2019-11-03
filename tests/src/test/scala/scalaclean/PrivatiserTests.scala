package scalaclean

class PrivatiserTests extends AbstractProjectTests {

  test("privatiser1") {
    privatiserProjectTest("privatiserProject1")
  }

  test("privatiser2") {
    privatiserProjectTest("privatiserProject2")
  }

  test("privatiser3") {
    privatiserProjectTest("privatiserProject3")
  }

  test("privatiser4") {
    privatiserProjectTest("privatiserProject4")
  }

  test("privatiser5") {
    privatiserProjectTest("privatiserProject5")
  }

  test("privatiser6-annotations") {
    privatiserProjectTest("privatiserProject6")
  }

  test("privatiser7") {
    privatiserProjectTest("privatiserProject7")
  }

}