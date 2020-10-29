package scalaclean.util

import java.io.{PrintStream, PrintWriter}
import java.nio.file.Path

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model.{SCPatch, SourceModel}
import scalaclean.rules.SourceFile

import scala.collection.mutable

class PatchStats(options: ScalaCleanCommandLine) {

  def issueCount = issues.size

  val debug = options.debug

  def printSummary(projectName: String): Unit = {
    println(s"""Files           Observed             = $filesVisited
               |Files           Skipped              = ${fileDoesntExist.size}
               |Files           Errors               = ${issues.size}
               |Files           Inconsistent         = ${inconsistentFiles}
               |Files           Changed              = ${filesChanged}
               |Files           Effect rate          = ${(filesChanged.toDouble / filesVisited.toDouble * 10000).toInt / 100} %
               |Elements        Observed             = $elementsVisited
               |Source Elements Observed             = $sourceElementsVisited
               |Source Elements Changed              = $elementsChanged
               |Source Elements Effect rate          = ${(elementsChanged.toDouble / sourceElementsVisited.toDouble * 10000).toInt / 100} %
               |""".stripMargin)
    fileDoesntExist.sorted.foreach(file => println(s"skipped nonexistent file $file"))
    issues.foreach { case (file, (message, error, patches)) =>
      println(s"error in file $file $message")
      error.printStackTrace(System.out)
      patches.foreach(println)
    }
  }

  private var filesVisited          = 0
  private var inconsistentFiles     = 0
  private var filesChanged          = 0
  private var elementsVisited       = 0
  private var sourceElementsVisited = 0
  private var elementsChanged       = 0

  private var fileDoesntExist: List[Path] = Nil
  private val issues                      = mutable.SortedMap[Path, (String, Throwable, List[SCPatch])]()

  def fileNotFound(sourceFile: SourceFile) = {
    fileDoesntExist ::= sourceFile.file.filename
  }

  def failedToGenerateFixes(sourceFile: SourceFile, t: Throwable) = {
    issues(sourceFile.file.filename) = ("Failed to generate", t, Nil)
  }

  def failedToApplyFixes(sourceFile: SourceFile, fixes: SingleFileVisit, t: Throwable) = {
    issues(sourceFile.file.filename) = ("Failed to apply", t, fixes.patches)
  }

  def appliedFixes(sourceFile: SourceFile, fixes: SingleFileVisit) = {
    if (fixes.patches.isEmpty) {
      if (debug)
        println(s"DEBUG: Not Overwriting [no changes] file: ${sourceFile.file}")
    } else {
      if (debug) {
        println(s"DEBUG: Overwriting file: ${sourceFile.file} with ${fixes.patches.size} changes")
        println("--------NEW----------")
        fixes.patches.foreach(println)
        println("------------------")

      }
    }
    filesChanged += 1
    elementsVisited += fixes.elementsVisited
    sourceElementsVisited += fixes.sourceElementsVisited
    elementsChanged += fixes.patches.size
  }

  def process(sourceFile: SourceFile) = {
    filesVisited += 1
  }
  val noException = new Exception("") {
    override def printStackTrace(s: PrintStream): Unit = ()
    override def printStackTrace(s: PrintWriter): Unit = ()
  }
  def sourceChanged(sourceFile: SourceFile, message: String, isIssue: Boolean) = {
    if (isIssue)
      issues(sourceFile.file.filename) = (message, noException, Nil)
    if (debug) {
      println(s"sourceChanged!!! ${sourceFile.file.filename}\n$message")
    }
    inconsistentFiles += 1
  }

}
