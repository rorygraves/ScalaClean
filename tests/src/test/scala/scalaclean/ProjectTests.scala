package scalaclean

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalaclean.cli.DeadCodeProjectTestRunner
import scalafix.testkit.DiffAssertions

class ProjectTests extends AssertionsForJUnit with DiffAssertions {

  def projectTest(projectName: String, overwriteTarget: Boolean = false): Unit = {
    projectTest(List(projectName), overwriteTarget)
  }

  def projectTest(projectNames: List[String], overwriteTarget: Boolean): Unit = {
    val res = new DeadCodeProjectTestRunner(projectNames, overwriteTarget).run()
    if(!res)
      fail(s" Failed for projects $projectNames, overwriteTarget=$overwriteTarget")
//    assert(res,s" Failed for projects $projectNames, overwriteTarget=$overwriteTarget")
  }


  @Test def deadCode1(): Unit = {
    projectTest("deadCodeProject1")
  }

  @Test def deadCode2() {
    projectTest("deadCodeProject2")
  }

  @Test def deadCode3() {
    projectTest("deadCodeProject3")
  }

  @Test def deadCode4() {
    projectTest("deadCodeProject4")
  }

  @Test def deadCode5() {
    projectTest("deadCodeProject5")
  }

  @Test def deadCode6() {
    projectTest(List("deadCodeProject6a", "deadCodeProject6b"), false)
  }
}