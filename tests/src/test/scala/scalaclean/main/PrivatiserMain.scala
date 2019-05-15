package scalafix.scalaclean.main

import java.nio.charset.StandardCharsets

import org.scalatest.exceptions.TestFailedException
import scalaclean.Analysis
import scalaclean.rules.privatiser.Privatiser
import scalafix.internal.config.ScalafixConfig
import scalafix.internal.diff.DiffDisable
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.internal.v1.LazyValue
import scalafix.lint.RuleDiagnostic
import scalafix.testkit.DiffAssertions
import scalafix.v1
import scalafix.v1.{Rule, SemanticDocument, SemanticRule}

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

  val targetFile = RelativePath("scalaclean/test/rules/privatiser/Private1.scala")
  val inputClasspath = Classpath("/workspace/ScalaClean/input/target/scala-2.12/classes:/Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:/Users/rorygraves/.ivy2/cache/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar")
  val sourceroot = AbsolutePath("/workspace/ScalaClean")
  val scalacOptions = List("-Yrangepos")
  val outputSourceDirectories: List[AbsolutePath] = Classpath("/workspace/ScalaClean/output/src/main/scala").entries
  val inputSourceDirectories: List[AbsolutePath] = Classpath("/workspace/ScalaClean/input/src/main/scala").entries
  val scalaVersionStr = "2.12.8"

  def semanticPatch(
    rule: Rule,
    sdoc: SemanticDocument,
    suppress: Boolean
  ): (String, List[RuleDiagnostic]) = {
    val fixes = (rule match {
      case rule: SemanticRule =>
        Some(rule.name -> rule.fix(sdoc))
      case _ =>
        None
    }).map(Map.empty + _).getOrElse(Map.empty)
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def run: Unit = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)

    // run analysis
    val analysis = new Analysis
//    analysis.beforeStart() - does nothing
    val sdoc1 = readSemanticDoc(classLoader, symtab)
    semanticPatch(analysis, sdoc1, suppress = false)
    analysis.afterComplete()

    // run privatiser
    val privatiser = new Privatiser()
    privatiser.beforeStart()
    val sdoc2 = readSemanticDoc(classLoader, symtab)
    val (fixed, messages) = semanticPatch(privatiser, sdoc2, suppress = false)
//    privatiser.afterComplete() - does nothing

    // compare results
    val tokens = fixed.tokenize.get
    val obtained = tokens.mkString

    val outputSourceDirectory = outputSourceDirectories.head
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
      throw new TestFailedException("see above", 0)
    }
  }

  def readSemanticDoc(
    //test: TestkitPath,
    classLoader: ClassLoader,
    symtab: SymbolTable): v1.SemanticDocument = {

    val sourceDirectory = inputSourceDirectories.head
    val testPath = targetFile
    val inputPath = sourceDirectory.resolve(testPath)
    val semanticdbPath = inputPath.toRelative(sourceroot)
//    val test = new TestkitPath(inputPath, testPath, semanticdbPath)

    val testInput = Input.VirtualFile(targetFile.toString, FileIO.slurp(inputPath, StandardCharsets.UTF_8))

    val input = testInput
    val tree = input.parse[Source].get
    val scalafixConfig = ScalafixConfig()
    val doc = v1.SyntacticDocument(
      tree.pos.input,
      LazyValue.now(tree),
      DiffDisable.empty,
      scalafixConfig
    )

    v1.SemanticDocument.fromPath(
      doc,
      semanticdbPath,
      classLoader,
      symtab)

  }
}