package scalaclean.cli

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, Paths}
import java.util.function.BiPredicate

import scalaclean.cli.FileHelper.toPlatform
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

import scala.meta.internal.io.FileIO
import scala.meta.{AbsolutePath, Classpath, RelativePath, _}

class DeadCodeProjectTestRunner(val projectName: String, useNew: Boolean, overwriteTargetFiles: Boolean) extends DiffAssertions {

  val scalaCleanWorkspace = "."
  val ivyDir: String = toPlatform("$HOME$/.ivy2/cache")


  val sourceRoot = AbsolutePath(scalaCleanWorkspace)
  val outputClassDir: String = toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/target/scala-2.12/classes/")
  val storagePath: String = toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/old/")
  val inputClasspath = Classpath(toPlatform(s"$outputClassDir:$ivyDir/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:$ivyDir/org.scalaz/scalaz-core_2.12/bundles/scalaz-core_2.12-7.2.27.jar"))
  val inputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$scalaCleanWorkspace/testProjects/$projectName/src/main/scala")).entries

  def sourceFiles(directories: List[AbsolutePath]): List[RelativePath] = {
    import scala.collection.JavaConverters._
    val files: List[RelativePath] = inputSourceDirectories.flatMap { sourceDir =>
      val sourceDirPath = sourceDir.toNIO
      if(sourceDir.isDirectory) {
        val absPaths = Files.find(sourceDir.toNIO,Integer.MAX_VALUE, new BiPredicate[Path, BasicFileAttributes] {
          override def test(
            t: Path, u: BasicFileAttributes): Boolean = { u.isRegularFile && t.getFileName.toString.endsWith(".scala") }
        }).iterator().asScala.toList
        absPaths.map(AbsolutePath(_).toRelative(sourceDir))
      } else
        Nil
    }
    files.sortBy(_.toString())
  }

  val targetFiles = sourceFiles(inputSourceDirectories)

  def semanticPatch(
    rule: AbstractRule,
    sdoc: SemanticDocument,
    suppress: Boolean
  ): (String, List[RuleDiagnostic]) = {
    val fixes = Some(RuleName(rule.name) -> rule.fix(sdoc)).map(Map.empty + _).getOrElse(Map.empty)
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def run(): Boolean = {

    // if we are in old mode generate the META-INF/ScalaClean/old analysis files
//    if(!useNew)
      AnalysisHelper.runAnalysis(projectName, inputClasspath, sourceRoot, inputSourceDirectories, outputClassDir, storagePath, targetFiles)
    runDeadCode()
  }

  def runDeadCode(): Boolean = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)

    println("---------------------------------------------------------------------------------------------------")
    // run DeadCode
    val rootDir = Paths.get("/workspace/ScalaClean")
    val srcDir = Paths.get(s"testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/")

    // if 'useNew' is enabled - load the data from the compiler plugin files not the old analysis files
    if(useNew) {
      val projects = new Projects(rootDir, "src" -> srcDir)
      ModelHelper.model = Some(projects)
    }

    val deadCode = new DeadCodeRemover()
    deadCode.beforeStart()


    targetFiles.foreach { targetFile =>
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
