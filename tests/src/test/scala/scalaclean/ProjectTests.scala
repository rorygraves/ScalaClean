package scalaclean

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalaclean.cli.DeadCodeProjectTestRunner
import scalafix.testkit.DiffAssertions

class ProjectTests extends AssertionsForJUnit with DiffAssertions {

  def projectTest(projectName: String, newCode: Boolean, overwriteTarget: Boolean = false): Unit = {
    val res = new DeadCodeProjectTestRunner(projectName, newCode, overwriteTarget).run()
    assert(res,s" Failed for project $projectName, newCode=$newCode, overwriteTarget=$overwriteTarget")
  }

  @Test def deadCode1Old() {
    projectTest("deadCodeProject1", newCode = false)
  }

  @Test def deadCode1New(): Unit = {
    projectTest("deadCodeProject1", newCode = true)
  }

  @Test def deadCode2Old() {
    projectTest("deadCodeProject2", newCode = false)
  }

  @Test def deadCode2New() {
    projectTest("deadCodeProject2", newCode = true)
  }

  @Test def deadCode3Old() {
    projectTest("deadCodeProject3", newCode = false)
  }

  @Test def deadCode3New() {
    projectTest("deadCodeProject3", newCode = true)
  }

}