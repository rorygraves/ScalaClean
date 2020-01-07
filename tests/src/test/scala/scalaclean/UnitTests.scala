package scalaclean

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import org.scalatest.FunSuite
import org.scalatestplus.junit.AssertionsForJUnit
import scalaclean.cli.FileHelper.toPlatform
import scalaclean.cli.{DiffAssertions, FileHelper, SCPatchUtil}
import scalaclean.model.ProjectModel
import scalaclean.model.impl.ProjectSet
import scalaclean.test._
import scalafix.scalaclean.cli.DocHelper
import scalafix.v1.SyntacticDocument

import scala.meta._
import scala.meta.internal.io.FileIO

trait AbstractUnitTests extends FunSuite with AssertionsForJUnit with DiffAssertions {

  def runTest(file: String, ruleFn: ProjectModel => TestCommon, overwrite: Boolean = false): Unit = {
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
                   origDocContents: String,
                   syntacticDocument: SyntacticDocument
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
        val syntacticDocument = DocHelper.readSyntacticDoc(absFile, targetFile)
        val obtained = applyRule(rule, targetFile.toString(), origFile, syntacticDocument)

        val targetOutput = RelativePath(targetFile.toString() + ".expected")
        val outputFile = inputSourceDirectories.head.resolve(targetOutput)
        val expected = FileIO.slurp(outputFile, StandardCharsets.UTF_8)


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

          fail
        }
      }
    }

    run()
  }
}

class UnitTests extends AbstractUnitTests {

  test("akkaTimeoutTest") {
    runTest("scalaclean/test/akka/Timeout.scala", new Test_allTransitiveOverrides(_))
  }

  test("internalOutgoingReferences") {
    runTest("scalaclean/test/references/internalOutgoingReferences/internalOutgoingReferences.scala", new Test_internalOutgoingReferences(_))
  }
}