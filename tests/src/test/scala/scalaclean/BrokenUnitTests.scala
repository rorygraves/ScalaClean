package scalaclean

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import scalaclean.cli.FileHelper
import scalaclean.cli.FileHelper.toPlatform
import scalaclean.model.ProjectModel
import scalaclean.model.impl.ProjectSet
import scalaclean.test._
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.scalaclean.cli.DocHelper
import scalafix.testkit.DiffAssertions
import scalafix.v1.SemanticDocument

import scala.meta._
import scala.meta.internal.io.FileIO

// These unit tests are asp
class BrokenUnitTests extends AbstractUnitTests {

  test("nodesTest") {
    runTest("scalaclean/test/nodes/nodes.scala", new TestNodes(_))
  }

  test("internalTransitiveOverriddenByTest") {
    runTest("scalaclean/test/overriddenBy/internalTransitiveOverriddenBy/internalTransitiveOverriddenBy.scala", new Test_internalTransitiveOverriddenBy(_))
  }

  test("internalDirectOverriddenBy") {
    runTest("scalaclean/test/overriddenBy/internalDirectOverriddenBy/internalDirectOverriddenBy.scala", new Test_internalTransitiveOverriddenBy(_))
  }

  test("allDirectOverrides") {
    runTest("scalaclean/test/overrides/allDirectOverrides/allDirectOverrides.scala", new Test_allDirectOverrides(_))
  }

  test("allTransitiveOverrides") {
    runTest("scalaclean/test/overrides/allTransitiveOverrides/allTransitiveOverrides.scala", new Test_allTransitiveOverrides(_))
  }

  test("internalDirectOverrides") {
    runTest("scalaclean/test/overrides/internalDirectOverrides/internalDirectOverrides.scala", new Test_internalDirectOverrides(_))
  }

  test("internalTransitiveOverrides") {
    runTest("scalaclean/test/overrides/internalTransitiveOverrides/internalTransitiveOverrides.scala", new Test_internalTransitiveOverrides(_))
  }

  test("allOutgoingReferences") {
    runTest("scalaclean/test/references/allOutgoingReferences/allOutgoingReferences.scala", new Test_allOutgoingReferences(_))
  }

  test("internalIncomingReferences") {
    runTest("scalaclean/test/references/internalIncomingReferences/internalIncomingReferences.scala", new Test_internalIncomingReferences(_))
  }

  test("annotations") {
    runTest("scalaclean/test/annotation/Annotated.scala",new Test_annotations(_))
  }
}