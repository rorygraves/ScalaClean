package scalaclean

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import org.scalatestplus.junit.AssertionsForJUnit
import scalaclean.cli.SCPatchUtil
import scalaclean.model.ProjectModel
import scalaclean.model.impl.ProjectSet
import scalaclean.test._
import scalaclean.util.FileHelper.toPlatform
import scalaclean.util.{DiffAssertions, FileHelper}

import scala.meta._
import scala.meta.internal.io.FileIO

trait AbstractUnitTests extends AnyFunSuite with AssertionsForJUnit with DiffAssertions with BeforeAndAfterAllConfigMap {
  private var overwrite = false

  override protected def beforeAll(configMap: ConfigMap): Unit = {
    overwrite = configMap.getWithDefault("overwrite", "false").equalsIgnoreCase("true")
  }

  def runTest(file: String, ruleFn: ProjectModel => TestCommon, expectationSuffix: String = "", overwrite: Boolean = false): Unit = {
    val projectName = "unitTestProject"
    val scalaCleanWorkspace = if (new File(toPlatform("../testProjects")).exists()) {
      ".."
    } else {
      "."
    }

    val targetFiles = List(
      RelativePath(file)
    )

    val outputClassDir: String = s"$scalaCleanWorkspace/testProjects/$projectName/target/scala-2.12/classes/"
    val inputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/src/main/scala")).entries

    def applyRule(
                   rule: TestCommon,
                   filename: String,
                   origDocContents: String
                 ): String = {

      rule.beforeStart()
      val patches = rule.run(filename)
      SCPatchUtil.applyFixes(origDocContents, patches)
    }

    def run(): Unit = {

      val classDir = outputClassDir + FileHelper.fileSep + "META-INF" + FileHelper.fileSep + "ScalaClean"
      val srcDir = Paths.get(classDir).toAbsolutePath

      val propsFile = srcDir.resolve("ScalaClean.properties")
      val projects = new ProjectSet(propsFile)

      runRule(projects)
    }

    def runRule(projectModel: ProjectModel): Unit = {

      println("---------------------------------------------------------------------------------------------------")
      // run rule

      val rule = ruleFn(projectModel)
      rule.beforeStart()
      targetFiles.foreach { targetFile =>
        val absFile = inputSourceDirectories.head.resolve(targetFile)
        val origFile = FileIO.slurp(absFile, StandardCharsets.UTF_8)
        val obtained = stripLocalIds(applyRule(rule, targetFile.toString(), origFile))

        val targetOutput = RelativePath(targetFile.toString() + expectationSuffix + ".expected")
        val outputFile = inputSourceDirectories.head.resolve(targetOutput)
        val expected = stripLocalIds(FileIO.slurp(outputFile, StandardCharsets.UTF_8))

        if (overwrite) {
          println("Overwriting target file: " + outputFile)
          val w = new FileOutputStream(outputFile.toString())
          w.write(obtained.getBytes(StandardCharsets.UTF_8))
          w.close()
        }

        val diff = DiffAssertions.compareContents(obtained, expected)
        if (diff.nonEmpty) {
          println("###########> obtained       <###########")
          println(obtained)
          println("###########> expected       <###########")
          println(expected)
          println("###########> Diff       <###########")
          println(error2message(obtained, expected))

          fail("Differences detected, see diff above")
        }
      }
    }

    run()
  }

  private val LocalIds = "/local([0-9]+)".r
  private def stripLocalIds(s: String) = LocalIds.replaceAllIn(s, "/localXXXXXXXX")
}


class UnitTests extends AbstractUnitTests with AssertionsForJUnit {
  test("nodesTest")  {
    runTest("scalaclean/test/nodes/nodes.scala", new TestNodes(_))
  }

  test("akkaTimeoutTest") {
    runTest("scalaclean/test/akka/Timeout.scala", new Test_allTransitiveOverrides(_))
  }

  test("internalOutgoingReferences") {
    runTest("scalaclean/test/references/internalOutgoingReferences/internalOutgoingReferences.scala", new Test_internalOutgoingReferences(_))
  }

  test("internalTransitiveOverriddenByTest") {
    runTest("scalaclean/test/overriddenBy/internalTransitiveOverriddenBy/internalTransitiveOverriddenBy.scala", new Test_internalTransitiveOverriddenBy(_))
  }

  test("internalDirectOverriddenBy") {
    runTest("scalaclean/test/overriddenBy/internalDirectOverriddenBy/internalDirectOverriddenBy.scala", new Test_internalDirectOverriddenBy(_))
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


  // test duplicate methods between obj/class, not really testing outgoing references.
  test("objClass") {
    runTest("scalaclean/test/objclass/ObjClass.scala", new Test_allOutgoingReferences(_))
  }

  test("allOutgoingReferences") {
    runTest("scalaclean/test/references/allOutgoingReferences/allOutgoingReferences.scala", new Test_allOutgoingReferences(_))
  }

  test("internalIncomingReferences") {
    runTest("scalaclean/test/references/internalIncomingReferences/internalIncomingReferences.scala", new Test_internalIncomingReferences(_))
  }

  test("annotations") {
    runTest("scalaclean/test/annotation/Annotated.scala",new TestExtensions(_))
  }
}
class PlugUnitTests extends AbstractUnitTests with AssertionsForJUnit {
  val file = File.createTempFile("banFromFile", "txt")
  val writer = Files.newBufferedWriter(file.toPath)
  writer.write("single,C:scalaclean.test.plugin.banFromFile.Class1,ban class 1,and,some text:::")
  writer.newLine()
  writer.write("pattern,.:scalaclean.test.plugin.banFromFile.Class2.*,ban class 2,and,some text:::")
  writer.newLine()
  writer.close

  test("banFromFile") {
    runTest("scalaclean/test/plugin/banFromFile/banFromFile.scala", new ShowColour(_, List(s"scalaclean.rules.plugin.BanFromFile:${file.getAbsolutePath}")))
  }

}
