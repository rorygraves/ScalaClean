package scalaclean

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalaclean.cli.DeadCodeProjectTestRunner
import scalafix.testkit.DiffAssertions

class ProjectTests extends AssertionsForJUnit with DiffAssertions {

  def projectTest(projectName: String, overwriteTarget: Boolean = false): Unit = {
    val res = new DeadCodeProjectTestRunner(projectName, overwriteTarget).run()
    assert(res,s" Failed for project $projectName, overwriteTarget=$overwriteTarget")
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
}