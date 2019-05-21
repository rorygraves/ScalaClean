package scalafix.scalaclean.main

import java.io.File
import java.nio.charset.StandardCharsets

import scalaclean.Analysis
import scalaclean.rules.AbstractRule
import scalaclean.rules.privatiser.Privatiser
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.rule.RuleName
import scalafix.testkit.DiffAssertions
import scalafix.v1
import scalafix.v1.{SemanticDocument, SyntacticDocument}

import scala.meta._
import scala.meta.inputs.Input
import scala.meta.internal.io.FileIO
import scala.meta.internal.symtab.SymbolTable

object PrivatiserMain {
  def main(args: Array[String]): Unit = {
    new PrivatiserMain().run
  }
}


class PrivatiserMain extends DiffAssertions {

  val storagePath = "/Users/rorygraves/Downloads/temp3"
  val targetFiles = List(
    RelativePath("scalaclean/test/rules/privatiser/Private1.scala"),
    RelativePath("scalaclean/test/rules/privatiser/Private2.scala"))
  val outputClassDir: String = "/workspace/ScalaClean/privatiser-test-input/target/scala-2.12/classes/"
  val inputClasspath = Classpath(s"$outputClassDir:/Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:/Users/rorygraves/.ivy2/cache/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar")
  val sourceRoot = AbsolutePath("/workspace/ScalaClean")
  val outputSourceDirectories: List[AbsolutePath] = Classpath("/workspace/ScalaClean/output/src/main/scala").entries
  val inputSourceDirectories: List[AbsolutePath] = Classpath("/workspace/ScalaClean/privatiser-test-input/src/main/scala").entries

  def semanticPatch(
    rule: AbstractRule,
    sdoc: SemanticDocument,
    suppress: Boolean
  ): (String, List[RuleDiagnostic]) = {
    val fixes = Some(RuleName(rule.name) -> rule.fix(sdoc)).map(Map.empty + _).getOrElse(Map.empty)
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def readSemanticDoc(
    classLoader: ClassLoader,
    symtab: SymbolTable,
    targetFile: RelativePath
  ): v1.SemanticDocument = {

    val sourceDirectory = inputSourceDirectories.head
    val inputPath = sourceDirectory.resolve(targetFile)
    val semanticdbPath = inputPath.toRelative(sourceRoot)

    val testInput = Input.VirtualFile(targetFile.toString, FileIO.slurp(inputPath, StandardCharsets.UTF_8))

    val input = testInput
    val doc = SyntacticDocument.fromInput(input)

    v1.SemanticDocument.fromPath(
      doc,
      semanticdbPath,
      classLoader,
      symtab)

  }

  def run: Unit = {

    runAnalysis
    runPrivatiser
  }

  def runAnalysis: Unit = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)

    // run analysis
    val analysis = new Analysis(storagePath, "privatiser-test-input",inputClasspath.entries.map(_.toString()).mkString(File.pathSeparator),
      outputClassDir,inputSourceDirectories.head.toRelative(sourceRoot).toString(),inputSourceDirectories.head.toString())

    targetFiles.foreach { targetFile =>
      println(s"Processing file: $targetFile")
      val sdoc = readSemanticDoc(classLoader, symtab, targetFile)
      analysis.analyse(sdoc)
    }
    analysis.afterComplete()
  }


  def runPrivatiser: Unit = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)
    val outputSourceDirectory = outputSourceDirectories.head

    println("---------------------------------------------------------------------------------------------------")
    // run privatiser
    val privatiser = new Privatiser()
    privatiser.beforeStart()
    targetFiles.foreach { targetFile =>
      val sdoc = readSemanticDoc(classLoader, symtab, targetFile)
      val (fixed, messages) = semanticPatch(privatiser, sdoc, suppress = false)
      //    privatiser.afterComplete() - does nothing

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
      }

      if (diff.nonEmpty) {
        throw new IllegalStateException("see above")
      }
    }
  }

}