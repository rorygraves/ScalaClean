package scalaclean.cli

import java.io.{PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scalaclean.model.ProjectModel
import scalaclean.model.impl.{Project, ProjectSet}
import scalaclean.rules.AbstractRule
import scalaclean.rules.deadcode.{DeadCodeRemover, SimpleDeadCode}
import scalaclean.rules.privatiser.{Privatiser, SimplePrivatiser}
import scalafix.internal.reflect.ClasspathOps
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
            model => new Privatiser(model, options.debug)
          case SCOptions.simplePrivatiserCmd =>
            model => new SimplePrivatiser(model, options.debug)
          case SCOptions.deadCodeCmd =>
            model => new DeadCodeRemover(model, options.debug)
          case SCOptions.simpleDeadCodeCmd =>
            model => new SimpleDeadCode(model, options.debug)
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

  def generateHTML(generated: String, altSource: String) = {
    import scala.io.Source
    val cssText : String = Source.fromResource("default-style.css").mkString

    writeToFile(AbsolutePath("/tmp/code.css"), cssText)
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    pw.println(
      """
        |<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        |<html xmlns="http://www.w3.org/1999/xhtml">
        |    <head>
        |        <title>XXXXX</title>
        |        <link rel="stylesheet" type="text/css" href="code.css" title="Style">
        |    </head>
        |    <body onload="initializeLinked()">
        |        <pre>
        |""".stripMargin)

    pw.println(generated)
    pw.println("--------------------")
    pw.println("--------------------")
    pw.println(altSource)
    pw.println("--------------------")

    pw.println(
      """        </pre>
        |    </body>
        |</html>
        |""".stripMargin
    )

    pw.flush()
    val str = sw.toString
    writeToFile(AbsolutePath("/tmp/code.html"), str)
//    Runtime.getRuntime.exec("open /tmp/code.html")
//    System.exit(1)


  }

  def semanticPatch(
                     rule: AbstractRule,
                     sdoc: SemanticDocument,
                     suppress: Boolean,
                     source: String,
                      altPathCode: String
                   ): String = {
    val fixes = rule.fix(sdoc)

    val sb = new StringBuilder
    var currentPos = 0
    var remaining = source

    println(s"-- ${remaining.length} remaining")

    fixes foreach { case (start,end,text) =>
      println(s" start $start  end $end text '$text' curPos = $currentPos  remaining= ${remaining.length}  buffer = ${sb.length}" )
      if(start > currentPos) {
        val diff = start-currentPos
        println(s"--  Taking $diff characters")
        sb.append(remaining.take(diff))
        remaining = remaining.drop(diff)
        currentPos = start
      }

      val toDrop = end - start
//        sb.append(">>>>>")
      println(s"--  dropping $toDrop chars - ${remaining.length} remaining '${remaining.take(toDrop)}''")
//        sb.append(remaining.take(toDrop))
      remaining = remaining.drop(toDrop)
//        sb.append("<<<<<")
      sb.append(text)
      currentPos = end
    }

    println("adding remaining " + remaining)
    sb.append(remaining)

    println("-------------------------")
    println(sb.toString())
    println("-------------------------")

    generateHTML(sb.toString(),  altPathCode)

    sb.toString()
  }

  def run(): Boolean = {

    val projectProps = dcOptions.files.map(f => Paths.get(f.toString))

    val projectSet = new ProjectSet(projectProps: _*)

    val rule = ruleCreateFn(projectSet)
    println(s"Running rule: ${rule.name}")

    rule.beforeStart()

    var changed = false
    projectSet.projects foreach { project =>
      changed |= runRuleOnProject(rule, project, dcOptions.validate, dcOptions.replace, dcOptions.debug)
    }
    if (dcOptions.debug)
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
  def runRuleOnProject(
    rule: AbstractRule, project: Project, validateMode: Boolean, replace: Boolean, debug: Boolean): Boolean = {

    val symtab: SymbolTable = ClasspathOps.newSymbolTable(project.classPath)
    val classLoader = project.classloader

    var changed = false

    println("---------------------------------------------------------------------------------------------------")

    val srcBase = AbsolutePath(project.src)
    val base = AbsolutePath(project.srcBuildBase)

    val files: Seq[AbsolutePath] = project.srcFiles.toList.map(AbsolutePath(_))

    def findRelativeSrc(
      absTargetFile: meta.AbsolutePath, basePaths: List[AbsolutePath]): (AbsolutePath, RelativePath) = {

      val nioTargetFile = absTargetFile.toNIO
      val baseOpt = basePaths.find(bp => nioTargetFile.startsWith(bp.toNIO))
      baseOpt.map(bp => (bp, absTargetFile.toRelative(bp))).getOrElse(throw new IllegalStateException(s"Unable to resolve source root for $absTargetFile"))
    }

    files.foreach { absTargetFile =>
      val (relBase, targetFile) = findRelativeSrc(absTargetFile, project.srcRoots)

      val existingFilePath = relBase.resolve(targetFile)
      val existingFile = FileIO.slurp(existingFilePath, StandardCharsets.UTF_8)

      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, absTargetFile, base, targetFile)

      val fixed = semanticPatch(rule, sdoc, suppress = false, existingFile, "fixed")

      val obtained = fixed // we don't need to retokenise this I think

      if (validateMode) {
        val expectedFile = expectedPathForTarget(relBase, targetFile)

        changed |= compareAgainstFile(expectedFile, obtained)
        if (replace) {
          // overwrite the '.expected' file
          val overwritePath = expectedPathForTarget(relBase, targetFile)
          writeToFile(overwritePath, obtained)
        }
      } else {
        if (replace) {
          // overwrite the base file
          val overwritePath = absTargetFile
          if (debug)
            println(s"DEBUG: Overwriting existing file: $overwritePath")
          writeToFile(overwritePath, obtained)
        } else {
          val expectedFile = relBase.resolve(targetFile)

          if (debug)
            println("DEBUG Comparing obtained vs " + expectedFile)

          // diff against original file
          changed |= compareAgainstFile(expectedFile, obtained)
        }
      }
    }
    rule.printSummary("ALL")
    changed
  }

}