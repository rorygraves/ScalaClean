package scalaclean.unit

import org.scalatestplus.junit.AssertionsForJUnit
import scalaclean.AbstractUnitTests
import scalaclean.model.{AllProjectsModel, ExtendsInternalReference}
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

  test("all1") {
    doRun(
      new Test_extends(None, Some(_ => true), "all", _),
      suffix = "all")
  }
  test("all2") {
    doRun(
      new Test_extends(None, None, "all", _),
      suffix = "all")
  }

  test("direct1") {
    doRun(
      new Test_extends(Some(true), None, "direct1", _),
      suffix = "direct")
  }

  test("direct2") {
    doRun(
      new Test_extends(None, Some(_.isDirect) , "direct2", _),
      suffix = "direct")
  }

  test("indirect") {
    doRun(
      new Test_extends(None, Some(!_.isDirect), "indirect", _),
      suffix = "indirect")
  }

  test("collectionName") {
    doRun(
      new Test_extends(None, Some( _.elementId.toString.contains("scala.collection")), "collection", _),
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

  test("all1") {
    doRun(
      new Test_extendsCompiled(None, Some(_ => true), "all1", _),
      suffix = "all")
  }
  test("all2") {
    doRun(
      new Test_extendsCompiled(None, None, "all2", _),
      suffix = "all")
  }

  test("direct1") {
    doRun(
      new Test_extendsCompiled(Some(true), None, "direct1", _),
      suffix = "direct")
  }

  test("direct2") {
    doRun(
      new Test_extendsCompiled(None, Some(e => e.isDirect), "direct2", _),
      suffix = "direct")
  }

  test("indirect") {
    doRun(
      new Test_extendsCompiled(None, Some((e: ExtendsInternalReference) => !e.isDirect), "indirect", _),
      suffix = "indirect")
  }

  test("collectionName") {
    doRun(
      new Test_extendsCompiled(None, Some((e: ExtendsInternalReference) => e.element.modelElementId.toString.contains("scala.collection")), "collection", _),
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

  test("all1") {
    doRun(
      new Test_extendedBy(None, None, "all", _),
      suffix = "all")
  }

  test("all2") {
    doRun(
      new Test_extendedBy(None, Some(_ => true), "all", _),
      suffix = "all")
  }

  test("direct1") {
    doRun(
      new Test_extendedBy(Some(true), None, "direct1", _),
      suffix = "direct")
  }

  test("direct2") {
    doRun(
      new Test_extendedBy(None, Some(_.isDirect), "direct2", _),
      suffix = "direct")
  }

  test("indirect") {
    doRun(
      new Test_extendedBy(None, Some(!_.isDirect), "indirect", _),
      suffix = "indirect")
  }

  test("marker") {
    doRun(
      new Test_extendedBy(None, Some(_.element.modelElementId.toString.contains("Marker")), "marker", _),
      suffix = "marker")
  }
}
