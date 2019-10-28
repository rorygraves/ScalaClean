package org.scalaclean.analysis

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.collection.mutable

// TODO This class should not be needed - use StringWriter instead
// however ElementWriter appears to be very sensitive to write order right now
class SortedStringWriter(targetPath: Path) {
  val target = Files.newBufferedWriter(targetPath,
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.CREATE)

  var strings = mutable.SortedSet[String]()

  def writeLine(s: String): Boolean = {
    val added = strings.add(s)
    added
  }

  def flush(): Unit = {
    strings.toVector.foreach { line =>
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