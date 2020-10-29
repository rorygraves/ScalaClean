package scalaclean

import scalaclean.cli.{AbstractProjectTestRunner, ScalaCleanCommandLine, SimpleRunOptions}
import scalaclean.model.AllProjectsModel
import scalaclean.rules.deadcode.{SimpleDeadCodeCommandLine, SimpleDeadCodeRemover}
import scalaclean.rules.finaliser.{Finaliser, FinaliserCommandLine}

class FinaliserTests extends AbstractProjectTests {
  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[FinaliserCommandLine, Finaliser](projectNames, options) {
      override def rule(cmd: FinaliserCommandLine, model: AllProjectsModel)= new Finaliser(cmd, model)
      override val expectationSuffix: String = ""
    }
    tester.run()
  }
  test("finaliser1") {
    projectTest("finaliserProject1")
  }

}
