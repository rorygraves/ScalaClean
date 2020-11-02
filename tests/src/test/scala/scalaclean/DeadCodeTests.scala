package scalaclean

import scalaclean.cli.{AbstractProjectTestRunner, SimpleRunOptions}
import scalaclean.model.AllProjectsModel
import scalaclean.rules.deadcode.{FullDeadCodeCommandLine, FullDeadCodeRemover, SimpleDeadCodeCommandLine, SimpleDeadCodeRemover}

class FullDeadCodeTests extends BaseDeadCodeTests {
  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[FullDeadCodeCommandLine, FullDeadCodeRemover](projectNames, options) {
      override def rule(cmd: FullDeadCodeCommandLine, model: AllProjectsModel)= new FullDeadCodeRemover(cmd, model)
      override val expectationSuffix = ".full"
    }
    tester.run()
  }
}

class SimpleDeadCodeTests extends BaseDeadCodeTests {

  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[SimpleDeadCodeCommandLine, SimpleDeadCodeRemover](projectNames, options) {
      override def rule(cmd: SimpleDeadCodeCommandLine, model: AllProjectsModel)= new SimpleDeadCodeRemover(cmd, model)
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

  test("deadCode12_isolated") {
    projectTest("deadCodeProject12_isolated")
  }

  test("deadCode13_case_class") {
    projectTest("deadCodeProject13_case_class")
  }

  test("deadCode14_anon_class") {
    projectTest("deadCodeProject14_anon_class")
  }

  test("deadCodeProject15_entry_point") {
    projectTest("deadCodeProject15_entry_point")
  }

  test("deadCodeProject16_params") {
    projectTest("deadCodeProject16_params")
  }

  test("deadCodeProject17_annotations") {
    projectTest("deadCodeProject17_annotations")
  }

  test("deadCodeProject18_utf8") {
    projectTest("deadCodeProject18_utf8")
  }

  test("deadCodeProject18_utf16") {
    projectTest("deadCodeProject18_utf16")
  }

  test("deadCodeProject18_utf32") {
    projectTest("deadCodeProject18_utf32")
  }

  test("deadCodeProject19_selftype") {
    projectTest("deadCodeProject19_selftype")
  }

  test("deadCodeProject20_overridevals") {
    projectTest("deadCodeProject20_overridevals", options = SimpleRunOptions.apply(replace = true))
  }

}
