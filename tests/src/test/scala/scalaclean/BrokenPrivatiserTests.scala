package scalaclean.broken

import scalaclean.AbstractProjectTests
import scalaclean.cli.{AbstractProjectTestRunner, ScalaCleanCommandLine, SimpleRunOptions}
import scalaclean.model.ProjectModel
import scalaclean.rules.privatiser.{FullPrivatiser, FullPrivatiserCommandLine, SimplePrivatiser, SimplePrivatiserCommandLine}


class BrokenFullPrivatiserTests extends BrokenPrivatiserTests {
  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[FullPrivatiserCommandLine, FullPrivatiser](projectNames, options) {
      override def rule(cmd: FullPrivatiserCommandLine, model: ProjectModel)= new FullPrivatiser(cmd, model)
      override val expectationSuffix = ".full"
    }
    tester.run()
  }
}
class BrokenFullPrivatiserNoDupTests extends BrokenPrivatiserTests {
  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[FullPrivatiserCommandLine, FullPrivatiser](projectNames, options) {

      override def customise(options: FullPrivatiserCommandLine): Unit = {
        super.customise(options)
        options.reduceDuplicateScopeChanges = true
      }

      override def rule(cmd: FullPrivatiserCommandLine, model: ProjectModel)= new FullPrivatiser(cmd, model)
      override val expectationSuffix = ".full-reduceDuplicate"
    }
    tester.run()
  }
}

class BrokenSimplePrivatiserTests extends BrokenPrivatiserTests {

  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[SimplePrivatiserCommandLine, SimplePrivatiser](projectNames, options) {
      override def rule(cmd: SimplePrivatiserCommandLine, model: ProjectModel)= new SimplePrivatiser(cmd, model)
      override val expectationSuffix: String = ".simple"
    }
    tester.run()
  }
}

class BrokenSimplePrivatiserNoDupTests extends BrokenPrivatiserTests {

  override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
    object tester extends AbstractProjectTestRunner[SimplePrivatiserCommandLine, SimplePrivatiser](projectNames, options) {

      override def customise(options: SimplePrivatiserCommandLine): Unit = {
        super.customise(options)
        options.reduceDuplicateScopeChanges = true
      }

      override def rule(cmd: SimplePrivatiserCommandLine, model: ProjectModel)= new SimplePrivatiser(cmd, model)
      override val expectationSuffix: String = ".simple-reduceDuplicate"
    }
    tester.run()
  }
}

// These tests are known to be broken - once they are working move them into PrivatiserTests
abstract class BrokenPrivatiserTests extends AbstractProjectTests {

    override def projectsTest(projectNames: List[String], options: SimpleRunOptions): Unit = {
      object tester extends AbstractProjectTestRunner[FullPrivatiserCommandLine, FullPrivatiser](projectNames, options) {
        override def rule(cmd: FullPrivatiserCommandLine, model: ProjectModel)= new FullPrivatiser(cmd, model)
        val expectationSuffix: String = ""
      }
      tester.run()
    }


  test("privatiser2") {
    projectTest("privatiserProject2")
  }

  test("privatiser4") {
    projectTest("privatiserProject4")
  }

  test("privatiser5") {
    projectTest("privatiserProject5")
  }
}
