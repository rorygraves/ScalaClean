package scalaclean.unit

import org.scalatestplus.junit.AssertionsForJUnit
import scalaclean.AbstractUnitTests
import scalaclean.model.AllProjectsModel
import scalaclean.test.{Test_extendedBy, Test_extends, Test_extendsCompiled}

object ExtendsTests {
  val src = "scalaclean/test/extends_/allExtends.scala"
  val overwrite = false
}
class ExtendsTests extends AbstractUnitTests with AssertionsForJUnit {

  def doRun(test: AllProjectsModel => Test_extends, suffix: String): Unit = {
    runTest(ExtendsTests.src,
      test,
      expectationSuffix = s".extends.$suffix",
      overwrite = ExtendsTests.overwrite)

  }

  test("all") {
    doRun(
      new Test_extends(false, (_, _, _) => true, "all", _),
      suffix = "all")
  }

  test("direct1") {
    doRun(
      new Test_extends(true, (_, _, _) => true, "direct1", _),
      suffix = "direct")
  }

  test("direct2") {
    doRun(
      new Test_extends(false, (direct, _, _) => direct, "direct2", _),
      suffix = "direct")
  }

  test("indirect") {
    doRun(
      new Test_extends(false, (direct, _, _) => !direct, "indirect", _),
      suffix = "indirect")
  }

  test("collectionName") {
    doRun(
      new Test_extends(false, (_, _, elementId) => elementId.toString.contains("scala.collection"), "collection", _),
      suffix = "collection")
  }
}

class ExtendsCompiledTests extends AbstractUnitTests {

  def doRun(test: AllProjectsModel => Test_extendsCompiled, suffix: String): Unit = {
    runTest(ExtendsTests.src,
      test,
      expectationSuffix = s".extends-compiled.$suffix",
      overwrite = ExtendsTests.overwrite)

  }

  test("all") {
    doRun(
      new Test_extendsCompiled(false, (_, _) => true, "all", _),
      suffix = "all")
  }

  test("direct1") {
    doRun(
      new Test_extendsCompiled(true, (_, _) => true, "direct1", _),
      suffix = "direct")
  }

  test("direct2") {
    doRun(
      new Test_extendsCompiled(false, (direct, _) => direct, "direct2", _),
      suffix = "direct")
  }

  test("indirect") {
    doRun(
      new Test_extendsCompiled(false, (direct, _) => !direct, "indirect", _),
      suffix = "indirect")
  }

  test("collectionName") {
    doRun(
      new Test_extendsCompiled(false, (_, cls) => cls.modelElementId.toString.contains("scala.collection"), "collection", _),
      suffix = "collection")
  }
}

class ExtendedByTests extends AbstractUnitTests {

  def doRun(test: AllProjectsModel => Test_extendedBy, suffix: String): Unit = {
    runTest(ExtendsTests.src,
      test,
      expectationSuffix = s".extendedBy.$suffix",
      overwrite = ExtendsTests.overwrite)

  }

  test("all") {
    doRun(
      new Test_extendedBy(false, (_, _) => true, "all", _),
      suffix = "all")
  }

  test("direct1") {
    doRun(
      new Test_extendedBy(true, (_, _) => true, "direct1", _),
      suffix = "direct")
  }

  test("direct2") {
    doRun(
      new Test_extendedBy(false, (direct, _) => direct, "direct2", _),
      suffix = "direct")
  }

  test("indirect") {
    doRun(
      new Test_extendedBy(false, (direct, _) => !direct, "indirect", _),
      suffix = "indirect")
  }

  test("marker") {
    doRun(
      new Test_extendedBy(false, (_, cls) => cls.modelElementId.toString.contains("Marker"), "marker", _),
      suffix = "marker")
  }
}
