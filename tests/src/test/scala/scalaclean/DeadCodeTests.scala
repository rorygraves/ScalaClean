package scalaclean

import org.scalatest.FunSuite
import scalaclean.cli.{DeadCodeProjectTestRunner, PrivatiserProjectTestRunner}

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
    deadCodeProjectTest(List("deadCodeProject6a", "deadCodeProject6b"), overwriteTarget = false)
  }

}