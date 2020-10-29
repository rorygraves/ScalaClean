package scalaclean.rules

import java.io.{PrintWriter, StringWriter}
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import scalaclean.cli.{SCPatchUtil, ScalaCleanCommandLine}
import scalaclean.model._
import scalaclean.model.impl.{ProjectImpl, ProjectSet}
import scalaclean.util.{DiffAssertions, PatchStats, SingleFileVisit}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3
import scala.util.{Failure, Success, Try}

abstract class AbstractRule[T <: ScalaCleanCommandLine] {
  type Rule <: RuleRun[T]
  def cmdLine: T
  def apply(options: T, model: AllProjectsModel): Rule

  def main(args: Array[String]): Unit = {
    val options = cmdLine
    parse(options, args)
    apply(options, new ProjectSet(options.files: _*)).run()
  }

  def parse(cmdLine: ScalaCleanCommandLine, args: Array[String]): Unit = {
    import org.kohsuke.args4j._
    val parser = new CmdLineParser(cmdLine)

    try { // parse the arguments.
      parser.parseArgument(args: _*)
    } catch {
      case e: CmdLineException =>
        System.err.println(e.getMessage)
        System.err.println(s"java ${getClass.getName} [options...] arguments...")
        parser.printUsage(System.err)
        System.err.println()
        sys.exit(1)
    }
  }

}

abstract class RuleRun[T <: ScalaCleanCommandLine] {
  val options: T
  val model: AllProjectsModel

  def name = getClass.getSimpleName

  private lazy val patchStats = new PatchStats(options)

  def debug = options.debug

  def addComments = options.addComments

  def printSummary(projectName: String): Unit = patchStats.printSummary(projectName)

  type Colour = Mark[SpecificColour]
  type SpecificColour <: SomeSpecificColour

  def dontChange(reason: String) = Mark.dontChange[SpecificColour](SimpleReason(reason))

  def makeChange(level: SpecificColour) = Mark.specific[SpecificColour](level)

  final def beforeStart(): Unit = {
    if (debug)
      println(s"$name performing analysis")

    markInitial()

    beforeRule()

    if (debug)
      println(s"$name running rule")

    runRule()
    afterRule()

    if (debug)
      debugDump()

    if (debug)
      println(s"$name analysis complete")
  }

  def beforeRule() = {
    if (debug)
      println(s"$name running ${options.rulePlugins} rule plugins before rule")
    options.rulePlugins.foreach { rule =>
      if (debug)
        println(s"$name running rule ${rule.name}  before main rule")
      rule.beforeMainRule(this)
    }
  }

  def afterRule() = {
    if (debug)
      println(s"$name running ${options.rulePlugins} rule plugins after rule")
    options.rulePlugins.foreach { rule =>
      if (debug)
        println(s"$name running rule ${rule.name}  before main rule")
      rule.afterMainRule(this)
    }
  }

  def debugDump(): Unit = {}

  def markInitial(): Unit = markAll[ModelElement](Mark.initial[SpecificColour])

  def runRule(): Unit

  def generateFixes(sourceFile: SourceFile): SingleFileVisit

  def markAll[E <: ModelElement : ClassTag](colour: Colour): Unit = {
    val all = model.allOf[E].toList
    model
      .allOf[E]
      .foreach(
        _.mark = colour
      )
  }

  implicit class Coloured(e: ModelElement) {
    def colour: Colour = e.mark.asInstanceOf[Colour]

    def colour_=(newColour: Colour): Unit = e.mark = newColour
  }

  //utility methods

  def allMainEntryPoints: Iterator[ModelElement] = {
    allMainMethodEntries ++ allApp
  }

  def allMainMethodEntries: Iterator[ModelElement] = {
    model.allOf[ObjectModel].filter(_.isTopLevel).flatMap { om =>
      om.methods.collect { case mm: PlainMethodModel if mm.methodName == "main" => mm }
    }
  }

  def allApp: Iterator[ObjectModel] = {
    val allAppObjects = ElementIds.allAppObjects
    for (obj <- model.allOf[ObjectModel] if allAppObjects.exists(appLike => obj.xtends(appLike)))
      yield obj

  }

  def otherAnnotationBasedEntryPoints: Iterator[ModelElement] = {
    model.allOf[ModelElement].filter { method =>
      method.annotations.exists(a => annotationEntryPoints.contains(a.fqName))
    }
  }

  protected val annotationEntryPoints = Set[String](
    "org.springframework.jmx.export.annotation.ManagedOperation",
    "org.springframework.jmx.export.annotation.ManagedAttribute"
  )

  def allTestEntryPoints: Iterator[ModelElement] = {
    allJunitTest ++ allJunitClasses ++ allScalaTests
  }

  private val junitAnnotationEntryPoints = Set(
    "org.junit.Ignore",
    "org.junit.Test",
    "org.junit.Before",
    "org.junit.After",
    "org.junit.BeforeClass",
    "org.junit.AfterClass",
  )

  def allJunitTest: Iterator[MethodModel] = {
    model.allOf[MethodModel].filter { method =>
      method.annotations.exists(a => junitAnnotationEntryPoints.contains(a.fqName))
    }
  }

  def allJunitClasses: Iterator[ClassLike] = {
    allJunitTest
      .map(_.classOrEnclosing)
      .toSet
      .flatMap { cls: ClassLike => cls.extendedByClassLike() ++ cls.extendsClassLikeCompiled() } iterator
  }

  def allScalaTests: Iterator[ClassLike] = {
    val suite = ElementIds("C:org.scalatest.Suite")
    model.allOf[ClassLike].filter { cls: ClassLike => cls.xtends(suite) }
  }

  def allSerialisationEntries: Iterator[MethodModel] = {
    model.allOf[MethodModel].filter { method =>
      (method.methodName == "writeObject" /*      && method.params == objectOutputStream */) ||
        (method.methodName == "readObject" /*       && method.params == objectInputStream */) ||
        (method.methodName == "readObjectNoData" /* && method.params == empty */) ||
        (method.methodName == "writeReplace" /*     && method.params == empty */) ||
        (method.methodName == "readResolve" /*      && method.params == empty */)
    }
    // ++
  }

  //  def generateHTML(generated: String, original: String): Unit = {
  //    import scala.io.Source
  //    val cssText: String = Source.fromResource("default-style.css").mkString
  //
  //    writeToFile(Paths.get("/tmp/code.css"), cssText)
  //    val sw = new StringWriter()
  //    val pw = new PrintWriter(sw)
  //    pw.println(
  //      """
  //        |<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
  //        |<html xmlns="http://www.w3.org/1999/xhtml">
  //        |    <head>
  //        |        <title>XXXXX</title>
  //        |        <link rel="stylesheet" type="text/css" href="code.css" title="Style">
  //        |    </head>
  //        |    <body onload="initializeLinked()">
  //        |        <pre>
  //        |""".stripMargin
  //    )
  //
  //    pw.println(generated)
  //    pw.println("--------------------")
  //    pw.println("--------------------")
  //    pw.println(original)
  //    pw.println("--------------------")
  //
  //    pw.println(
  //      """        </pre>
  //        |    </body>
  //        |</html>
  //        |""".stripMargin
  //    )
  //
  //    pw.flush()
  //    val str = sw.toString
  //    writeToFile(Paths.get("/tmp/code.html"), str)
  //    //    Runtime.getRuntime.exec("open /tmp/code.html")
  //    //    System.exit(1)
  //
  //  }

  def run(): Boolean = {
    val projectSet = model match {
      case projectSet: ProjectSet => projectSet
      case _ => new ProjectSet(options.files: _*)
    }
    println(s"Running rule: $name")

    beforeStart()

    var changed = false
    projectSet.projects.foreach { project =>
      if (patchStats.issueCount <= options.maxIssues) {
        changed |= runRuleOnProject(project)
      }
    }
    if (options.debug)
      println(s"DEBUG: Changed = $changed")
    printSummary("ALL")
    changed
  }

  object testSupport extends DiffAssertions {

    def expectedPathForTarget(src: SourceModel): Path = {
      src.filename.resolveSibling(src.filename.getFileName + options.testOptions.expectationSuffix + ".expected")
    }

    def diffString(diff: String, expectedFile: Path, p1: (String, String, Boolean), p2: (String, String, Boolean)) : String = {
      val sb = new StringBuilder()
      val (p1Value, p1Name, p1Show) = p1
      val (p2Value, p2Name, p2Show) = p2
      if (p1Show) {
        sb.append(s"###########> $p1Name       <###########\n")
        println(p1Value)
      }
      if (p2Show) {
        sb.append(s"###########> $p2Name       <###########\n")
        println(p2Value)
      }
      sb.append("###########> Diff       <###########\n")
      sb.append(error2message(diff))

      sb.toString()
    }
    def compareAgainstFile(debug: Boolean, expectedFile: Path, expected: String, obtained: String): Boolean = {

      val diff = DiffAssertions.compareContents(obtained, expected)
      if (diff.nonEmpty) {
        println(diffString(diff, expectedFile,(obtained, "obtained", true), (expected, "expected", true)))
        true
      } else {
        if (debug)
          println("DEBUG Comparing obtained vs " + expectedFile)
        false
      }
    }

  }

  def writeToFile(path: Path, content: String): Unit = {
    Files.write(path, content.getBytes)
  }

  def validateSource(sourceFile: SourceFile): Boolean = {
    def isChanged(): String = {
      val changed = new StringBuilder
      val content = sourceFile.content
      if (content.length != sourceFile.file.sourceLength)
        changed append s"Source validation failed for ${sourceFile.file.filename} : length found ${content.length} expected ${sourceFile.file.sourceLength}\n"
      if (content.hashCode != sourceFile.file.sourceJavaHash)
        changed append s"Source validation failed for ${sourceFile.file.filename} : hash found ${content.hashCode} expected ${sourceFile.file.sourceJavaHash}"
      if (MurmurHash3.stringHash(content) != sourceFile.file.sourceMurmurHash) {
        changed append s"Source validation failed for ${sourceFile.file.filename} : MurmurHash3 found ${MurmurHash3.stringHash(content)} expected ${sourceFile.file.sourceMurmurHash}"
      }
      if (changed.nonEmpty) {
        sourceFile.file.project.originalSource(sourceFile.file) foreach {
          original =>
            changed append "\n"
            val diff = DiffAssertions.compareContents(content, original)
            changed append testSupport.diffString(diff, sourceFile.file.filename, (original, "compiled", true), (content, "current", true))
        }

      }
      changed.toString()
    }

    import scalaclean.cli.SourceValidation
    options.sourceValidation match {
      case SourceValidation.NONE => true
      case SourceValidation.SKIP =>
        val changed = isChanged()
        if (!changed.isEmpty) patchStats.sourceChanged(sourceFile, changed, false)
        changed.isEmpty
      case SourceValidation.FAIL =>
        val changed = isChanged()
        if (!changed.isEmpty) patchStats.sourceChanged(sourceFile, changed, true)
        changed.isEmpty
    }
  }

  /**
    * @param project The target project
    * @return True if diffs were seen or files were changed
    */
  def runRuleOnProject(
                        project: ProjectImpl
                      ): Boolean = {

    var changed = false

    if (debug)
      println("---------------------------------------------------------------------------------------------------")

    val files = model.allOf[SourceModel]

    files.foreach { file =>
      if (patchStats.issueCount <= options.maxIssues) {
        val sourceFile = new SourceFile(file)
        patchStats.process(sourceFile)
        if (!Files.exists(file.filename)) {
          patchStats.fileNotFound(sourceFile)
          if (!options.skipNonexistentFiles)
            throw new IllegalStateException(s"cant find file $file")
        } else if (validateSource(sourceFile))

          Try(generateFixes(sourceFile)) match {
            case Failure(t) => patchStats.failedToGenerateFixes(sourceFile, t)
            case Success(fixes) =>
              lazy val obtained = SCPatchUtil.applyFixes(sourceFile.content, fixes.patches)

              lazy val expectedFile: Path = testSupport.expectedPathForTarget(file)

              def expectedContent = new String(Files.readAllBytes(expectedFile), Charset.forName(file.encoding))

              if (options.testOptions.validate) {
                patchStats.appliedFixes(sourceFile, fixes)
                changed |= testSupport.compareAgainstFile(debug, expectedFile, expectedContent, obtained)
                if (options.replace) {
                  // overwrite the '.expected' file
                  writeToFile(expectedFile, obtained)
                }
              } else {
                if (options.replace) {
                  // overwrite the base file
                  if (fixes.patches.nonEmpty) {
                    Try(obtained) match {
                      case Failure(f) => patchStats.failedToApplyFixes(sourceFile, fixes, f)
                      case Success(obtained) =>
                        patchStats.appliedFixes(sourceFile, fixes)
                        if (debug)
                          println(s"DEBUG: Overwriting existing file: ${file.filename} with ${fixes.patches.size} changes")
                        writeToFile(file.filename, obtained)
                    }
                  }

                } else {
                  patchStats.appliedFixes(sourceFile, fixes)
                  // diff against original file
                  changed |= testSupport.compareAgainstFile(debug, expectedFile, expectedContent, obtained)
                }
              }
          }
      }
    }
    changed
  }

}
