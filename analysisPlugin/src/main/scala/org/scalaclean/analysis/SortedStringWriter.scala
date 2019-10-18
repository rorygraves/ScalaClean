package org.scalaclean.analysis

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.collection.mutable

class SortedStringWriter(targetPath: Path) {
  val target = Files.newBufferedWriter(targetPath,
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.CREATE)

  var strings = mutable.SortedSet[String]()

  def writeLine(s: String): Boolean = {
    strings.add(s)
  }

  def flush(): Unit = {
    strings.foreach { line =>
      target.write(line)
      target.newLine()
    }
    strings.clear
    target.flush()
  }

  def close(): Unit = {
    flush()
    target.close()
  }
}