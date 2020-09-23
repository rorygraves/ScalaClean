package scalaclean.util

object DiffAssertions {
  def compareContents(original: String, revised: String): String = {
    def splitLines(s: String) =
      s.trim.replaceAllLiterally("\r\n", "\n").split("\n")
    compareContents(splitLines(original), splitLines(revised))
  }

  def compareContents(original: Seq[String], revised: Seq[String]): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "obtained",
          "expected",
          original.asJava,
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

  def error2message(obtained: String, expected: String): String = {
    val sb = new StringBuilder
    if (obtained.length < 1000) {
      sb.append(s"""
                   #${header("Obtained")}
                   #${trailingSpace(obtained)}
         """.stripMargin('#'))
    }
    sb.append(s"""
                 #${header("Diff")}
                 #${trailingSpace(compareContents(obtained, expected))}
         """.stripMargin('#'))
    sb.toString()
  }

  def trailingSpace(str: String): String = str.replaceAll(" \n", "∙\n")

  def compareContents(original: String, revised: String): String =
    DiffAssertions.compareContents(original, revised)
}
