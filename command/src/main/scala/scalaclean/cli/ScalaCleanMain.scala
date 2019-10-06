package scalaclean.cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scalaclean.model.ProjectModel
import scalaclean.model.v3.{Project, ProjectSet}
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


object ScalaCleanMain {
  def main(args: Array[String]): Unit = {
    SCOptions.parseCommandLine(args) match {
      case Some(options) =>
        val commandFn: ProjectModel => AbstractRule = options.mode match {
          case SCOptions.privatiserCmd =>
            model =>  new DeadCodeRemover(model, options.debug)
          case SCOptions.deadCodeCmd =>
            model =>  new DeadCodeRemover(model, options.debug)
          case _ =>
            throw new IllegalStateException(s"Invalid command argument ${options.mode}")
        }
        new ScalaCleanMain(options, commandFn).run()
      case None =>
        System.exit(0)
    }
  }
}


class ScalaCleanMain(dcOptions: SCOptions, ruleCreateFn: ProjectModel => AbstractRule) extends DiffAssertions {

  def semanticPatch(
    rule: AbstractRule,
    sdoc: SemanticDocument,
    suppress: Boolean
  ): (String, List[RuleDiagnostic]) = {
    val fixes = Some(RuleName(rule.name) -> rule.fix(sdoc)).map(Map.empty + _).getOrElse(Map.empty)
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def run(): Boolean = {

    val projectProps = dcOptions.files.map(f => Paths.get(f.toString))

    val projectSet = new ProjectSet(projectProps :_*)

    val rule = ruleCreateFn(projectSet)
    println(s"Running rule: ${rule.name}")

    rule.beforeStart()

    var changed = false
    projectSet.projects foreach { project =>
      changed |= runRuleOnProject(rule, project, dcOptions.validate, dcOptions.replace, dcOptions.debug)
    }
    if(dcOptions.debug)
      println(s"DEBUG: Changed = $changed")
    changed
  }

  def expectedPathForTarget(srcBase: AbsolutePath, targetFile: RelativePath): AbsolutePath = {
    val targetOutput = RelativePath(targetFile.toString() + ".expected")
    val outputFile = srcBase.resolve(targetOutput)
    outputFile
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
    Files.write(path.toNIO, content.getBytes)
  }

  /**
    *
    * @param rule The rule to run
    * @param project The target project
    * @return True if diffs were seen or files were changed
    */
  def runRuleOnProject(rule: AbstractRule, project: Project, validateMode: Boolean, replace: Boolean, debug: Boolean): Boolean = {

    val symtab: SymbolTable = ClasspathOps.newSymbolTable(project.classPath)
    val classLoader = project.classloader

    var changed = false

    println("---------------------------------------------------------------------------------------------------")

    val srcBase = AbsolutePath(project.src)
    val base = AbsolutePath(project.srcBuildBase)

    val files = project.srcFiles.toList.map(AbsolutePath(_).toRelative(srcBase))

    files.foreach { targetFile =>

      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, srcBase, base, targetFile)
      val (fixed, _) = semanticPatch(rule, sdoc, suppress = false)

      // compare results
      val tokens = fixed.tokenize.get
      val obtained = tokens.mkString

      if(validateMode) {
        val expectedFile = expectedPathForTarget(srcBase, targetFile)

        changed |= compareAgainstFile(expectedFile, obtained)
        if(replace) {
          // overwrite the '.expected' file
          val overwritePath = expectedPathForTarget(srcBase, targetFile)
          writeToFile(overwritePath, obtained)
        }
      } else {
        if(replace) {
          // overwrite the base file
          val overwritePath = srcBase.resolve(targetFile)
          if(debug)
            println(s"DEBUG: Overwriting existing file: $overwritePath")
          writeToFile(overwritePath, obtained)
        } else {
          val expectedFile = srcBase.resolve(targetFile)

          if(debug)
            println("DEBUG Comparing obtained vs " + expectedFile)

          // diff against original file
          changed |= compareAgainstFile(expectedFile, obtained)
        }
      }
    }
    changed
  }

}