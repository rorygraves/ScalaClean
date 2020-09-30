package scalaclean

class DeadCodeTests extends  AbstractProjectTests {

  test("deadCode1") {
    deadCodeProjectTest("deadCodeProject1")
  }

  test("deadCode2") {
    deadCodeProjectTest("deadCodeProject2")
  }

  test("deadCode3") {
    deadCodeProjectTest("deadCodeProject3")
  }

  test("deadCode4") {
    deadCodeProjectTest("deadCodeProject4")
  }

  test("deadCode5") {
    deadCodeProjectTest("deadCodeProject5")
  }

  test("deadCode6") {
    deadCodeProjectTests(List("deadCodeProject6a", "deadCodeProject6b"))
  }

  test("deadCode7-inheritance") {
    deadCodeProjectTest("deadCodeProject7")
  }

  test("deadCode8") {
    deadCodeProjectTest("deadCodeProject8")
  }

  test("deadCode9-naming") {
    deadCodeProjectTest("deadCodeProject9")
  }
  test("deadCode10_vals") {
    deadCodeProjectTest("deadCodeProject10_vals")
  }

  test("deadCode11_constants") {
    deadCodeProjectTest("deadCodeProject11_constants")
  }

}
