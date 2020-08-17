package scalaclean.cli

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scalaclean.model.ProjectModel
import scalaclean.model.impl.{ Project, ProjectSet }
import scalaclean.rules.{ AbstractRule, NamedRule }
import scalaclean.rules.deadcode.DeadCodeRemover
import scalaclean.rules.privatiser.Privatiser
import scalafix.RuleName
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.scalaclean.cli.DocHelper
import scalafix.testkit.DiffAssertions
import scalafix.v1.SemanticDocument

import scala.meta._
import scala.meta.internal.io.FileIO
import scala.meta.internal.symtab.SymbolTable


object ScalaCleanMain extends DiffAssertions {
  def main(args: Array[String]): Unit = {
    SCOptions.parseCommandLine(args) match {
      case Some(options) =>
        val commandFn: ProjectModel => AbstractRule = options.mode match {
          case SCOptions.privatiserCmd =>
            model => new Privatiser(model, options.debug)
          case SCOptions.deadCodeCmd =>
            model => new DeadCodeRemover(model, options.debug)
          case _ =>
            throw new IllegalStateException(s"Invalid command argument ${options.mode}")
        }
        new ScalaCleanMain(options, commandFn).run()
      case None =>
        System.exit(0)
    }
  }

  def semanticPatch(
    rule: NamedRule,
    sdoc: SemanticDocument,
    suppress: Boolean
  ): (String, List[RuleDiagnostic]) = {
    val fixes = Map(RuleName(rule.ruleName) -> rule.fix(sdoc))
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def compareAgainstFile(existingFile: AbsolutePath, obtained: String): Boolean = {
    val expected = FileIO.slurp(existingFile, StandardCharsets.UTF_8)

    val diff = DiffAssertions.compareContents(obtained, expected)
    if (diff.nonEmpty) {
      println("###########> obtained       <###########")
      println(obtained)
      println("###########> expected       <###########")
      println(expected)
      println("###########> Diff       <###########")
      println(error2message(obtained, expected))
      true
    } else
      false
  }

  def writeToFile(path: AbsolutePath, content: String): Unit = {
    Files.write(path.toNIO, content.getBytes(StandardCharsets.UTF_8))
  }

  /**
    *
    * @param rule The rule to run
    * @param project The target project
    * @return True if diffs were seen or files were changed
    */
  def runRuleOnProject(
    rule: NamedRule, project: Project, files: Seq[AbsolutePath], validateMode: Boolean, replace: Boolean, debug: Boolean
  )(postProcess: String => String): Boolean = {
    val symtab: SymbolTable = ClasspathOps.newSymbolTable(project.classPath)
    val classLoader = project.classloader

    println("---------------------------------------------------------------------------------------------------")

    val base = AbsolutePath(project.srcBuildBase)
    files.foldLeft(false) { (anyChanged, absTargetFile) =>
      val targetFile = project.srcRoots.collectFirst {
        case b if absTargetFile.toNIO.startsWith(b.toNIO) => absTargetFile.toRelative(base)
      }.getOrElse(throw new IllegalStateException(s"Unable to resolve source root for $absTargetFile"))

      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, absTargetFile, base, targetFile)
      val (fixed, _) = semanticPatch(rule, sdoc, suppress = false)

      // compare results
      val tokens = fixed.tokenize.get
      val obtained = postProcess(tokens.mkString)

      if (validateMode) {
        val expectedFile = absTargetFile.resolveSibling(s => s"$s.expected")

        val changed = compareAgainstFile(expectedFile, obtained)
        if (replace) {
          // overwrite the '.expected' file
          writeToFile(expectedFile, obtained)
        }
        anyChanged | changed
      } else if (replace) {
        // overwrite the base file
        if (debug)
          println(s"DEBUG: Overwriting existing file: $absTargetFile")
        writeToFile(absTargetFile, obtained)
        anyChanged
      } else {
        if (debug)
          println(s"DEBUG Comparing obtained vs $absTargetFile")

        // diff against original file
        anyChanged | compareAgainstFile(absTargetFile, obtained)
      }
    }
  }
}

class ScalaCleanMain(opts: SCOptions, ruleCreateFn: ProjectModel => AbstractRule) {
  import ScalaCleanMain._

  def run(): Boolean = {
    val projectSet = new ProjectSet(opts.files.map(_.toPath): _*)
    val rule = ruleCreateFn(projectSet)

    println(s"Running rule: ${rule.name}")
    rule.beforeStart()

    val changed = projectSet.projects.foldLeft(false) { (anyChanged, project) =>
      val files: Seq[AbsolutePath] = project.srcFiles.toList.map(AbsolutePath(_))
      val changed = runRuleOnProject(rule, project, files, opts.validate, opts.replace, opts.debug)(s => s)
      rule.printSummary()
      anyChanged | changed
    }

    if (opts.debug) println(s"DEBUG: Changed = $changed")
    changed
  }
}
