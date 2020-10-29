package scalaclean

import scalaclean.cli.{AbstractProjectTestRunner, ScalaCleanCommandLine, SimpleRunOptions}
import scalaclean.model.AllProjectsModel
import scalaclean.rules.privatiser.{FullPrivatiser, FullPrivatiserCommandLine, SimplePrivatiser, SimplePrivatiserCommandLine}


class FullPrivatiserTests extends BasePrivatiserTests {
  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[FullPrivatiserCommandLine, FullPrivatiser](projectNames, options) {
      override def rule(cmd: FullPrivatiserCommandLine, model: AllProjectsModel)= new FullPrivatiser(cmd, model)
      override val expectationSuffix = ".full"
    }
    tester.run()
  }
}
class FullPrivatiserNoDupTests extends BasePrivatiserTests {
  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[FullPrivatiserCommandLine, FullPrivatiser](projectNames, options) {

      override def customise(options: FullPrivatiserCommandLine): Unit = {
        super.customise(options)
        options.reduceDuplicateScopeChanges = true
      }

      override def rule(cmd: FullPrivatiserCommandLine, model: AllProjectsModel)= new FullPrivatiser(cmd, model)
      override val expectationSuffix = ".full-reduceDuplicate"
    }
    tester.run()
  }
}

class SimplePrivatiserTests extends BasePrivatiserTests {

  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[SimplePrivatiserCommandLine, SimplePrivatiser](projectNames, options) {
      override def rule(cmd: SimplePrivatiserCommandLine, model: AllProjectsModel)= new SimplePrivatiser(cmd, model)
      override val expectationSuffix: String = ".simple"
    }
    tester.run()
  }
}

class SimplePrivatiserNoDupTests extends BasePrivatiserTests {

  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[SimplePrivatiserCommandLine, SimplePrivatiser](projectNames, options) {

      override def customise(options: SimplePrivatiserCommandLine): Unit = {
        super.customise(options)
        options.reduceDuplicateScopeChanges = true
      }

      override def rule(cmd: SimplePrivatiserCommandLine, model: AllProjectsModel)= new SimplePrivatiser(cmd, model)
      override val expectationSuffix: String = ".simple-reduceDuplicate"
    }
    tester.run()
  }
}

abstract class BasePrivatiserTests extends AbstractProjectTests {

  test("privatiser1") {
    projectTest("privatiserProject1")
  }

  test("privatiser3") {
    projectTest("privatiserProject3")
  }

//  test("privatiser3+") {
//    projectTest("privatiserProject3", options = SimpleRunOptions(debug = true, addComments = true))
//  }

  test("privatiser6-annotations") {
    projectTest("privatiserProject6")
  }

  test("privatiser7") {
    projectTest("privatiserProject7")
  }

}
