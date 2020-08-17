package scalaclean

import java.nio.file.Paths

import org.scalatest.{ BeforeAndAfterAllConfigMap, ConfigMap }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.AssertionsForJUnit
import scalaclean.cli.ScalaCleanMain.runRuleOnProject
import scalaclean.cli.AbstractProjectTestRunner.testProjectPropsFile
import scalaclean.model.impl.{ Project, ProjectSet }
import scalaclean.model.ProjectModel
import scalaclean.test._
import scalafix.testkit.DiffAssertions

import scala.meta._

class UnitTests extends AnyFunSuite with AssertionsForJUnit with DiffAssertions with BeforeAndAfterAllConfigMap {
  private var overwrite = false

  override protected def beforeAll(configMap: ConfigMap) = {
    overwrite = configMap.getWithDefault("overwrite", "false").equalsIgnoreCase("true")
  }

  test("akkaTimeoutTest") {
    runTest("scalaclean/test/akka/Timeout.scala", new Test_allTransitiveOverrides(_))
  }

  test("nodesTest") (pendingUntilFixed { // IllegalArgumentException: Unexpected symbol ... GetterMethodModel ... expecting ... VarModel
    runTest("scalaclean/test/nodes/nodes.scala", new TestNodes(_))
  })

  test("internalTransitiveOverriddenBy") {
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

  test("internalOutgoingReferences") {
    runTest("scalaclean/test/references/internalOutgoingReferences/internalOutgoingReferences.scala",new Test_internalOutgoingReferences(_))
  }

  test("annotations") {
    runTest("scalaclean/test/annotation/Annotated.scala",new Test_annotations(_))
  }

  def runTest(file: String, ruleCreateFn: ProjectModel => TestCommon, overwrite: Boolean = false): Unit = {
    val projectSet = new ProjectSet(testProjectPropsFile("unitTestProject").toNIO)
    val rule       = ruleCreateFn(projectSet)
    val replace    = UnitTests.this.overwrite || overwrite

    rule.beforeStart()

    def runRule(project: Project): Boolean = {
      val absTargetFile = project.srcFiles.collectFirst {
        case f if f.endsWith(file) => AbsolutePath(f)
      }.getOrElse(throw new IllegalStateException(s"Unable to find source root for $file in ${project.srcFiles}"))

      val files = List(absTargetFile)
      runRuleOnProject(rule, project, files, validateMode = true, replace = replace, debug = true)(stripLocalIds)
    }

    if (projectSet.projects.foldLeft(false)(_ | runRule(_)))
      fail
  }

  private val LocalIds = "/local([0-9]+)".r
  private def stripLocalIds(s: String) = LocalIds.replaceAllIn(s, "/localXXXXXXXX")
}
