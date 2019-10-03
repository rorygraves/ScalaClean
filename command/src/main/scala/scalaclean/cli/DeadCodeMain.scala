package scalaclean.cli

import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import scalaclean.cli.v3.Projects
import scalaclean.model.ModelHelper
import scalaclean.rules.AbstractRule
import scalaclean.rules.deadcode.DeadCodeRemover
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.rule.RuleName
import scalafix.scalaclean.cli.DocHelper
import scalafix.testkit.DiffAssertions
import scalafix.v1.SemanticDocument

import scala.meta._
import scala.meta.internal.io.FileIO
import scala.meta.internal.symtab.SymbolTable

object DeadCodeMain {
  def main(args: Array[String]): Unit = {
    new DeadCodeMain().run()
  }
}

// TODO - Share with PriviatiserMain - make it generic
class DeadCodeMain extends DiffAssertions {
  import scalaclean.cli.FileHelper.toPlatform

  val projectName = "deadCodeProject1"
  val scalaCleanWorkspace = "."
  val ivyDir: String = toPlatform("$HOME$/.ivy2/cache")
  val storagePath: String = toPlatform("$HOME$/Downloads/temp3")

  val targetFiles = List(
    RelativePath("scalaclean/test/rules/deadcode/DeadCodeAnnotation.scala"),
//    RelativePath("scalaclean/test/rules/deadcode/DeadCodeImport.scala"),
    RelativePath("scalaclean/test/rules/deadcode/DeadCodeMain.scala"),
    RelativePath("scalaclean/test/rules/deadcode/DeadCodeVarVal.scala")
  )
  val sourceRoot = AbsolutePath(scalaCleanWorkspace)
  val outputClassDir: String = toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/target/scala-2.12/classes/")
  val inputClasspath = Classpath(toPlatform(s"$outputClassDir:$ivyDir/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:$ivyDir/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar"))
  val inputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/src/main/scala")).entries

  def semanticPatch(
    rule: AbstractRule,
    sdoc: SemanticDocument,
    suppress: Boolean
  ): (String, List[RuleDiagnostic]) = {
    val fixes = Some(RuleName(rule.name) -> rule.fix(sdoc)).map(Map.empty + _).getOrElse(Map.empty)
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def run(): Unit = {
    val rootDir = Paths.get(".") // sourceRoot?

    val srcDir = Paths.get(outputClassDir)

    // TODO HACK
    val projects = new Projects(rootDir, "src" -> srcDir)
    ModelHelper.model = Some(projects)

    println("Running dead code removal")
    runDeadCode()
  }

  def runDeadCode(): Unit = {

    val symtab: SymbolTable = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)

    val elementsRelUrl = s"META-INF/ScalaClean/scalaclean-elements.csv"
    println("---------------------------------------------------------------------------------------------------")
    // run DeadCode
    val deadCode = new DeadCodeRemover()
    deadCode.beforeStart()
    targetFiles.foreach { targetFile =>
      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, inputSourceDirectories.head, sourceRoot, targetFile)
      val (fixed, _) = semanticPatch(deadCode, sdoc, suppress = false)

      // compare results
      val tokens = fixed.tokenize.get
      val obtained = tokens.mkString

      val targetOutput = RelativePath(targetFile.toString() + ".expected")
      val outputFile = inputSourceDirectories.head.resolve(targetOutput)
      val expected = FileIO.slurp(outputFile, StandardCharsets.UTF_8)

      val diff = DiffAssertions.compareContents(obtained, expected)
      if (diff.nonEmpty) {
        println("###########> obtained       <###########")
        println(obtained)
        println("###########> expected       <###########")
        println(expected)
        println("###########> Diff       <###########")
        println(error2message(obtained, expected))

        System.out.flush()
        System.exit(1)
      }
    }
  }

}