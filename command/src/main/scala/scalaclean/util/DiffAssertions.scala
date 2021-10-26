package scalaclean.util

import java.util
import java.util.{List => JList}

object DiffAssertions {

  def compareContents(original: String, originalName: String, revised: String, revisedName: String): String = {
    def splitLines(s: String) = {
      util.Arrays.asList(s.trim.split("\r?\n") :_*)
    }
    compareContents(splitLines(original), originalName, splitLines(revised), revisedName)
  }

  def compareContents(original: JList[String], originalName: String, revised: JList[String], revisedName: String): String = {
    import scala.jdk.CollectionConverters._
    val diff = difflib.DiffUtils.diff(original, revised)
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          originalName,
          revisedName,
          original,
          diff,
          1
        )
        .asScala
        .mkString("\n")
  }

}

trait DiffAssertions {

  def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  case class DiffFailure(
      title: String,
      expected: String,
      obtained: String,
      diff: String
  ) extends Exception

  def error2message(diff: String): String = {
    val sb = new StringBuilder
    sb.append(s"""
                 #${header("Diff")}
                 #${trailingSpace(diff)}
         """.stripMargin('#'))
    sb.toString()
  }

  def trailingSpace(str: String): String = str.replaceAll(" \n", "∙\n")

  def compareContents(original: String, revised: String): String = DiffAssertions.compareContents(original, "original", revised, "revised")
}
