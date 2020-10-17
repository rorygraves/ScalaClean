package scalaclean

import scalaclean.cli.{AbstractProjectTestRunner, SimpleRunOptions}
import scalaclean.model.ProjectModel
import scalaclean.rules.deadcode.{FullDeadCodeCommandLine, FullDeadCodeRemover, SimpleDeadCodeCommandLine, SimpleDeadCodeRemover}

class FullDeadCodeTests extends BaseDeadCodeTests {
  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[FullDeadCodeCommandLine, FullDeadCodeRemover](projectNames, options) {
      override def rule(cmd: FullDeadCodeCommandLine, model: ProjectModel)= new FullDeadCodeRemover(cmd, model)
      override val expectationSuffix = ".full"
    }
    tester.run()
  }
}

class SimpleDeadCodeTests extends BaseDeadCodeTests {

  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[SimpleDeadCodeCommandLine, SimpleDeadCodeRemover](projectNames, options) {
      override def rule(cmd: SimpleDeadCodeCommandLine, model: ProjectModel)= new SimpleDeadCodeRemover(cmd, model)
      override val expectationSuffix: String = ".simple"
    }
    tester.run()
  }
}

abstract class BaseDeadCodeTests extends AbstractProjectTests {


  test("deadCode1") {
    projectTest("deadCodeProject1")
  }

  test("deadCode2") {
    projectTest("deadCodeProject2")
  }

  test("deadCode3") {
    projectTest("deadCodeProject3")
  }

  test("deadCode4") {
    projectTest("deadCodeProject4")
  }

  test("deadCode5") {
    projectTest("deadCodeProject5")
  }

  test("deadCode6") {
    projectsTest(List("deadCodeProject6a", "deadCodeProject6b"))
  }

  test("deadCode7-inheritance") {
    projectTest("deadCodeProject7")
  }

  test("deadCode8") {
    projectTest("deadCodeProject8")
  }

  test("deadCode9-naming") {
    projectTest("deadCodeProject9")
  }
  test("deadCode10_vals") {
    projectTest("deadCodeProject10_vals")
  }

  test("deadCode11_constants") {
    projectTest("deadCodeProject11_constants")
  }

}
