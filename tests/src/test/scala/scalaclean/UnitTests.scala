package scalaclean

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import scalaclean.cli.FileHelper
import scalaclean.cli.FileHelper.toPlatform
import scalaclean.model.impl.ProjectSet
import scalaclean.model.ProjectModel
import scalaclean.test._
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.scalaclean.cli.DocHelper
import scalafix.testkit.DiffAssertions
import scalafix.v1.{SemanticDocument, SyntacticDocument}

import scala.meta._
import scala.meta.internal.io.FileIO

trait AbstractUnitTests extends FunSuite with AssertionsForJUnit with DiffAssertions {

  def runTest(file: String, ruleFn: ProjectModel => TestCommon, overwrite: Boolean = false): Unit = {
    val projectName = "unitTestProject"
    //    val scalaCleanWorkspace = ".."
    val ivyDir = toPlatform("$HOME$/.ivy2/cache")

    val scalaCleanWorkspace = if (new File(toPlatform("../testProjects")).exists()) {
      ".."
    } else {
      "."
    }

    val targetFiles = List(
      RelativePath(file)
    )

    val outputClassDir: String = s"${scalaCleanWorkspace}/testProjects/$projectName/target/scala-2.12/classes/"
    val inputClasspath = Classpath(toPlatform(s"$outputClassDir|$ivyDir/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar|$ivyDir/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar"))
    val sourceRoot = AbsolutePath(scalaCleanWorkspace)
    val inputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/src/main/scala")).entries

    def semanticPatch(
                       rule: TestCommon,
                       syntacticDocument: SyntacticDocument,
                       semanticDocument: SemanticDocument,
                       suppress: Boolean
                     ): (String, List[RuleDiagnostic]) = {
      val fixes = Some(rule.name -> rule.fix(semanticDocument)).map(Map.empty + _).getOrElse(Map.empty)
      PatchInternals.semantic(fixes, semanticDocument, suppress)
    }

    def run(): Unit = {

      val classDir = outputClassDir + FileHelper.fileSep + "META-INF" + FileHelper.fileSep + "ScalaClean"
      val srcDir = Paths.get(classDir).toAbsolutePath

      val propsFile = srcDir.resolve("ScalaClean.properties")
      val projects = new ProjectSet(propsFile)

      runRule(projects)
    }

    def runRule(projectModel: ProjectModel): Unit = {

      val symtab = ClasspathOps.newSymbolTable(inputClasspath)
      val classLoader = ClasspathOps.toClassLoader(inputClasspath)

      println("---------------------------------------------------------------------------------------------------")
      // run rule

      val rule = ruleFn(projectModel)
      rule.beforeStart()
      targetFiles.foreach { targetFile =>
        val absFile = inputSourceDirectories.head.resolve(targetFile)
        val (syntacticDocument, semanticDocument) = DocHelper.readSemanticDoc(classLoader, symtab, absFile, sourceRoot, targetFile)
        val (fixed, messages) = semanticPatch(rule, syntacticDocument, semanticDocument, suppress = false)

        // compare results
        val tokens = fixed.tokenize.get
        val obtained = tokens.mkString

        val targetOutput = RelativePath(targetFile.toString() + ".expected")
        val outputFile = inputSourceDirectories.head.resolve(targetOutput)

        if (overwrite) {
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

    run()
  }
}

class UnitTests extends AbstractUnitTests {

  test("akkaTimeoutTest") {
    runTest("scalaclean/test/akka/Timeout.scala", new Test_allTransitiveOverrides(_))
  }

  test("internalOutgoingReferences") {
    runTest("scalaclean/test/references/internalOutgoingReferences/internalOutgoingReferences.scala",new Test_internalOutgoingReferences(_))
  }
}