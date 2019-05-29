package scalaclean

import java.io.FileOutputStream
import java.nio.charset.StandardCharsets

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalaclean.cli.AnalysisHelper
import scalaclean.cli.FileHelper.toPlatform
import scalaclean.test.{TestCommon, Test_allDirectOverrides, Test_allOutgoingReferences, Test_allTransitiveOverrides, Test_internalDirectOverrides, Test_internalIncomingReferences, Test_internalOutgoingReferences, Test_internalTransitiveOverriddenBy, Test_internalTransitiveOverrides, Test_nodes}
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.scalaclean.cli.DocHelper
import scalafix.testkit.DiffAssertions
import scalafix.v1.SemanticDocument

import scala.meta._
import scala.meta.internal.io.FileIO

class UnitTests extends AssertionsForJUnit with DiffAssertions {

  @Test def akkaTimeoutTest() {
    runTest("scalaclean/test/akka/Timeout.scala", new Test_allTransitiveOverrides(), overwrite = true)
  }

  @Test def nodesTest() {
    runTest("scalaclean/test/nodes/nodes.scala", new Test_nodes())
  }

  @Test def internalTransitiveOverriddenByTest() {
    runTest("scalaclean/test/overriddenBy/internalTransitiveOverriddenBy/internalTransitiveOverriddenBy.scala", new Test_internalTransitiveOverriddenBy())
  }

  @Test def internalDirectOverriddenBy() {
    runTest("scalaclean/test/overriddenBy/internalDirectOverriddenBy/internalDirectOverriddenBy.scala", new Test_internalTransitiveOverriddenBy())
  }

  @Test def allDirectOverrides() {
    runTest("scalaclean/test/overrides/allDirectOverrides/allDirectOverrides.scala", new Test_allDirectOverrides())
  }

  @Test def allTransitiveOverrides() {
    runTest("scalaclean/test/overrides/allTransitiveOverrides/allTransitiveOverrides.scala", new Test_allTransitiveOverrides())
  }

  @Test def internalDirectOverrides() {
    runTest("scalaclean/test/overrides/internalDirectOverrides/internalDirectOverrides.scala", new Test_internalDirectOverrides())
  }

  @Test def internalTransitiveOverrides() {
    runTest("scalaclean/test/overrides/internalTransitiveOverrides/internalTransitiveOverrides.scala", new Test_internalTransitiveOverrides())
  }

  @Test def allOutgoingReferences() {
    runTest("scalaclean/test/references/allOutgoingReferences/allOutgoingReferences.scala", new Test_allOutgoingReferences())
  }

  @Test def internalIncomingReferences() {
    runTest("scalaclean/test/references/internalIncomingReferences/internalIncomingReferences.scala", new Test_internalIncomingReferences())
  }

  @Test def internalOutgoingReferences() {
    runTest("scalaclean/test/references/internalOutgoingReferences/internalOutgoingReferences.scala", new Test_internalOutgoingReferences())
  }


  def runTest(file: String, rule: TestCommon, overwrite: Boolean = false): Unit = {
    val projectName = "unitTestProject"
    val scalaCleanWorkspace = "."
    val ivyDir = toPlatform("$HOME$/.ivy2/cache")
    val storagePath = toPlatform("$HOME$/Downloads/temp3")

    val targetFiles = List(
      RelativePath(file),
    )
    val outputClassDir: String = s"/workspace/ScalaClean/testProjects/$projectName/target/scala-2.12/classes/"
    val inputClasspath = Classpath(s"$outputClassDir:$ivyDir/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:$ivyDir/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar")
    val sourceRoot = AbsolutePath(scalaCleanWorkspace)
    val inputSourceDirectories: List[AbsolutePath] = Classpath(s"$scalaCleanWorkspace/testProjects/$projectName/src/main/scala").entries

    def semanticPatch(
      rule: TestCommon,
      sdoc: SemanticDocument,
      suppress: Boolean
    ): (String, List[RuleDiagnostic]) = {
      val fixes = Some(rule.name -> rule.fix(sdoc)).map(Map.empty + _).getOrElse(Map.empty)
      PatchInternals.semantic(fixes, sdoc, suppress)
    }

    def run: Unit = {

      AnalysisHelper.runAnalysis(projectName, inputClasspath, sourceRoot,  inputSourceDirectories, outputClassDir, storagePath, targetFiles)
      runRule()
    }

    def runRule(): Unit = {

      val symtab = ClasspathOps.newSymbolTable(inputClasspath)
      val classLoader = ClasspathOps.toClassLoader(inputClasspath)

      println("---------------------------------------------------------------------------------------------------")
      // run rule

      rule.beforeStart()
      targetFiles.foreach { targetFile =>
        val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, inputSourceDirectories.head, sourceRoot, targetFile)
        val (fixed, messages) = semanticPatch(rule, sdoc, suppress = false)

        // compare results
        val tokens = fixed.tokenize.get
        val obtained = tokens.mkString

        val targetOutput = RelativePath(targetFile.toString() + ".expected")
        val outputFile = inputSourceDirectories.head.resolve(targetOutput)

        if(overwrite) {
                  println("Overwriting target file: " + outputFile)
                  val w = new FileOutputStream(outputFile.toString())
                  w.write(obtained.getBytes(StandardCharsets.UTF_8))
                  w.close()
        }

        val expected = FileIO.slurp(outputFile, StandardCharsets.UTF_8)

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
    run
  }

}