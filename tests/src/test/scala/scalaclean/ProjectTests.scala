package scalaclean

import org.scalatest.FunSuite
import scalaclean.cli.{DeadCodeProjectTestRunner, PrivatiserProjectTestRunner}

class ProjectTests extends FunSuite {

  def deadCodeProjectTest(projectName: String, overwriteTarget: Boolean = false): Unit = {
    deadCodeProjectTest(List(projectName), overwriteTarget)
  }

  def deadCodeProjectTest(projectNames: List[String], overwriteTarget: Boolean): Unit = {
    val res = new DeadCodeProjectTestRunner(projectNames, overwriteTarget).run()
    if(!res)
      fail(s" Failed for projects $projectNames, overwriteTarget=$overwriteTarget")
  }

  def privatiserProjectTest(projectName: String, overwriteTarget: Boolean = false): Unit = {
    privatiserProjectTest(List(projectName), overwriteTarget)
  }

  def privatiserProjectTest(projectNames: List[String], overwriteTarget: Boolean): Unit = {
    val res = new PrivatiserProjectTestRunner(projectNames, overwriteTarget).run()
    if(!res)
      fail(s" Failed for projects $projectNames, overwriteTarget=$overwriteTarget")
  }

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

}