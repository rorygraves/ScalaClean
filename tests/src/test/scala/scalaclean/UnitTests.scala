package scalaclean

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import org.scalatestplus.junit.AssertionsForJUnit
import scalaclean.cli.SCPatchUtil
import scalaclean.model.AllProjectsModel
import scalaclean.model.impl.ProjectSet
import scalaclean.test._
import scalaclean.util.DiffAssertions


trait AbstractUnitTests extends AnyFunSuite with AssertionsForJUnit with DiffAssertions with BeforeAndAfterAllConfigMap {
  private var overwrite = false

  override protected def beforeAll(configMap: ConfigMap): Unit = {
    overwrite = configMap.getWithDefault("overwrite", "false").equalsIgnoreCase("true")
  }

  def runTest(file: String, ruleFn: AllProjectsModel => TestCommon, expectationSuffix: String = "", overwrite: Boolean = false): Unit = {
    val scalaCleanWorkspace = if (Files.exists(Paths.get("../testProjects"))) {
      Paths.get("..").toAbsolutePath.toRealPath()
    } else {
      Paths.get(".").toAbsolutePath.toRealPath()
    }

    val projectDir = scalaCleanWorkspace.resolve(s"testProjects/unitTestProject/")
    val srcRoot = projectDir.resolve(s"src/main/scala/")
    val targetFiles = List(srcRoot.resolve(file))
    val propsFile = projectDir.resolve("target/scala-2.12/classes/META-INF/ScalaClean/ScalaClean.properties")
    val projects = new ProjectSet(propsFile)

    runRule(projects)

    def applyRule(
                   rule: TestCommon,
                   file: Path,
                   origDocContents: String
                 ): String = {

      rule.beforeStart()
      val patches = rule.run(file).patches
      SCPatchUtil.applyFixes(origDocContents, patches)
    }

    def runRule(projectModel: AllProjectsModel): Unit = {

      println("---------------------------------------------------------------------------------------------------")
      // run rule

      val charset = StandardCharsets.UTF_8

      val rule = ruleFn(projectModel)
      rule.beforeStart()
      targetFiles.foreach { targetFile =>
        val origFile = new String(Files.readAllBytes(targetFile), charset)
        val obtained = applyRule(rule, targetFile, origFile)

        val targetOutput = targetFile.resolveSibling(targetFile.getFileName.toString() + expectationSuffix + ".expected")
        val expected = new String(Files.readAllBytes(targetOutput), charset)

        if (overwrite) {
          println("Overwriting target file: " + targetOutput)
          val w = Files.newBufferedWriter(targetOutput, charset)
          w.write(obtained)
          w.close()
        }

        val diff = DiffAssertions.compareContents(obtained, "obtained", expected, "expected")
        if (diff.nonEmpty) {
          println("###########> obtained       <###########")
          println(obtained)
          println("###########> expected       <###########")
          println(expected)
          println("###########> Diff       <###########")
          println(error2message(diff))

          fail("Differences detected, see diff above")
        }
      }
    }
  }
}


class UnitTests extends AbstractUnitTests with AssertionsForJUnit {
//  -  var y2: Int = _/* VarModel C:scalaclean.test.nodes.GrandParentTrait/V:y2 [193-208] <method> <accessor> <defaultinit>  [[C:scalaclean.test.nodes.GrandParentTrait/V:y2]] *//* GetterMethodModel C:scalaclean.test.nodes.GrandParentTrait/M:y2() [193-208] <method> <accessor> <defaultinit>  [[C:scalaclean.test.nodes.GrandParentTrait/M:y2()]] */
//  +  var y2: Int/* GetterMethodModel C:scalaclean.test.nodes.GrandParentTrait/M:y2() [193-204] <method> <accessor> <defaultinit>  [[C:scalaclean.test.nodes.GrandParentTrait/M:y2()]] */ = _/* VarModel C:scalaclean.test.nodes.GrandParentTrait/V:y2 [193-208] <method> <accessor> <defaultinit>  [[C:scalaclean.test.nodes.GrandParentTrait/V:y2]] */

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
    runTest("scalaclean/test/references/allOutgoingReferences/allOutgoingReferences.scala", new Test_allOutgoingReferences(_), overwrite = true)
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
