package org.scalaclean.analysis

import java.nio.file.{Files, Path, StandardOpenOption}

import scala.collection.mutable


class StringWriter(targetPath: Path) {
  private val target = Files.newBufferedWriter(targetPath,
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.CREATE)

  def writeLine(line: String): Boolean = {
    target.write(line)
    target.newLine()
    true
  }

  def flush(): Unit = {
    target.flush()
  }

  def close(): Unit = {
    flush()
    target.close()
  }
}