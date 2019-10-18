package scalaclean

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalaclean.cli.{DeadCodeProjectTestRunner, PrivatiserProjectTestRunner}
import scalafix.testkit.DiffAssertions

class ProjectTests extends AssertionsForJUnit with DiffAssertions {

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

  @Test def deadCode1(): Unit = {
    deadCodeProjectTest("deadCodeProject1")
  }

  @Test def deadCode2() {
    deadCodeProjectTest("deadCodeProject2")
  }

  @Test def deadCode3() {
    deadCodeProjectTest("deadCodeProject3")
  }

  @Test def deadCode4() {
    deadCodeProjectTest("deadCodeProject4")
  }

  @Test def deadCode5() {
    deadCodeProjectTest("deadCodeProject5")
  }

  @Test def deadCode6() {
    deadCodeProjectTest(List("deadCodeProject6a", "deadCodeProject6b"), false)
  }

  @Test def privatiser1() {
    privatiserProjectTest("privatiserProject1")
  }

  @Test def privatiser2() {
    privatiserProjectTest("privatiserProject2")
  }

  @Test def privatiser3() {
    privatiserProjectTest("privatiserProject3")
  }

  @Test def privatiser4() {
    privatiserProjectTest("privatiserProject4")
  }

  @Test def privatiser5() {
    privatiserProjectTest("privatiserProject5")
  }

}