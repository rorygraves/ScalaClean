package scalaclean.cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scalaclean.cli.FileHelper.toPlatform
import scalaclean.model.ModelHelper
import scalaclean.model.v3.ProjectSet
import scalaclean.rules.AbstractRule
import scalaclean.rules.deadcode.DeadCodeRemover
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.rule.RuleName
import scalafix.scalaclean.cli.DocHelper
import scalafix.testkit.DiffAssertions
import scalafix.v1.SemanticDocument

import scala.meta.internal.io.FileIO
import scala.meta.{AbsolutePath, Classpath, RelativePath, _}

class DeadCodeProjectTestRunner(val projectName: String, overwriteTargetFiles: Boolean) extends DiffAssertions {

  val scalaCleanWorkspace = "."
  val ivyDir: String = toPlatform("$HOME$/.ivy2/cache")


  val sourceRoot = AbsolutePath(scalaCleanWorkspace)
  val outputClassDir: String = toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/target/scala-2.12/classes/")
  val storagePath: String = toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/old/")
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

  def run(): Boolean = {

    runDeadCode()
  }

  def runDeadCode(): Boolean = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)

    println("---------------------------------------------------------------------------------------------------")
    // run DeadCode
    val rootDir = Paths.get(".")

    val srcDir = Paths.get(s"testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/").toAbsolutePath

    val projectProps = srcDir.resolve(s"ScalaClean.properties")

    val projects = new ProjectSet(projectProps)
    ModelHelper.model = Some(projects)

    val deadCode = new DeadCodeRemover(projects)
    deadCode.beforeStart()

    val project = projects.projects.head
    val srcBase = AbsolutePath(project.src)

    val files = project.srcFiles.toList.map(AbsolutePath(_).toRelative(srcBase))

    files.foreach { targetFile =>
      println(s" processing $targetFile")
      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, inputSourceDirectories.head, sourceRoot, targetFile)
      val (fixed, _) = semanticPatch(deadCode, sdoc, suppress = false)

      // compare results
      val tokens = fixed.tokenize.get
      val obtained = tokens.mkString

      val targetOutput = RelativePath(targetFile.toString() + ".expected")
      val outputFile = inputSourceDirectories.head.resolve(targetOutput)

      if(overwriteTargetFiles)
        Files.write(outputFile.toNIO,obtained.getBytes)

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
        return false
      }
    }
    true
  }

}
