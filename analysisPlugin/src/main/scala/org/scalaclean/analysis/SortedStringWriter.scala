package org.scalaclean.analysis

import java.nio.file.{Files, Path, StandardOpenOption}

class SortedStringWriter(targetPath: Path) {
  val target = Files.newBufferedWriter(targetPath,
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.CREATE)

  var strings: Vector[String] = Vector.empty
  var current = ""

  def write(s: String): Unit = {
    current = current + s
  }

  def writeLine(s: String): Unit = {
    current = current + s
    newLine()
  }

  def newLine(): Unit = {
    if (current.nonEmpty) {
      strings = strings :+ current
      current = ""
    }
  }

  def flush(): Unit = {
    newLine()
    strings.toSet.toList.sorted.foreach { line =>
      target.write(line)
      target.newLine()
    }
    strings = Vector.empty
    target.flush()
  }

  def close(): Unit = {
    flush()
    target.close()
  }
}