package scalaclean

import org.scalatest.FunSuite
import scalaclean.cli.{DeadCodeProjectTestRunner, PrivatiserProjectTestRunner}

abstract class AbstractProjectTests extends FunSuite {
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


}
