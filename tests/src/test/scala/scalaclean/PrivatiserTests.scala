package scalaclean

class PrivatiserTests extends AbstractProjectTests {

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
