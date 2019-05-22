package scalaclean.cli

import java.nio.charset.StandardCharsets

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

object DeadCodeMain {
  def main(args: Array[String]): Unit = {
    new DeadCodeMain().run
  }
}

class DeadCodeMain extends DiffAssertions {
  import scalaclean.cli.FileHelper.toPlatform

  val projectName = "deadCodeProject1"
//  val scalaCleanWorkspace = "."
//  val ivyDir = "$HOME$/.ivy2/cache"
//  val storagePath = "$HOME$/Downloads/temp3"
  val scalaCleanWorkspace = "/workspace/ScalaClean"
  val ivyDir = "/Users/rorygraves/.ivy2/cache"
  val storagePath = "/Users/rorygraves/Downloads/temp3"

  val targetFiles = List(
    RelativePath("scalaclean/test/rules/deadcode/DeadCodeAnnotation.scala"),
    RelativePath("scalaclean/test/rules/deadcode/DeadCodeImport.scala"),
    RelativePath("scalaclean/test/rules/deadcode/DeadCodeMain.scala"),
    RelativePath("scalaclean/test/rules/deadcode/DeadCodeVarVal.scala")
  )
  val sourceRoot = AbsolutePath(scalaCleanWorkspace)
//  val outputClassDir: String = toPlatform(s"/workspace/ScalaClean/testProjects/$projectName/target/scala-2.12/classes/")
//  val inputClasspath = Classpath(toPlatform(s"$outputClassDir:$ivyDir/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:$ivyDir/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar"))
//  val outputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$scalaCleanWorkspace/output/src/main/scala")).entries
//  val inputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/src/main/scala")).entries
  val outputClassDir: String = s"/workspace/ScalaClean/testProjects/$projectName/target/scala-2.12/classes/"
  val inputClasspath = Classpath(s"$outputClassDir:$ivyDir/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:$ivyDir/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar")
  val outputSourceDirectories: List[AbsolutePath] = Classpath(s"$scalaCleanWorkspace/output/src/main/scala").entries
  val inputSourceDirectories: List[AbsolutePath] = Classpath(s"$scalaCleanWorkspace/testProjects/$projectName/src/main/scala").entries


  def semanticPatch(
    rule: AbstractRule,
    sdoc: SemanticDocument,
    suppress: Boolean
  ): (String, List[RuleDiagnostic]) = {
    val fixes = Some(RuleName(rule.name) -> rule.fix(sdoc)).map(Map.empty + _).getOrElse(Map.empty)
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def run: Unit = {

    AnalysisHelper.runAnalysis(projectName, inputClasspath, sourceRoot,  inputSourceDirectories, outputClassDir, storagePath, targetFiles)
    runDeadCode()
  }

  def runDeadCode(): Unit = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)
    val outputSourceDirectory = outputSourceDirectories.head

    println("---------------------------------------------------------------------------------------------------")
    // run DeadCode
    val deadCode = new DeadCodeRemover()
    deadCode.beforeStart()
    targetFiles.foreach { targetFile =>
      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, inputSourceDirectories.head, sourceRoot, targetFile)
      val (fixed, messages) = semanticPatch(deadCode, sdoc, suppress = false)

      // compare results
      val tokens = fixed.tokenize.get
      val obtained = tokens.mkString

      val outputFile = outputSourceDirectory.resolve(targetFile)
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