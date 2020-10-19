package org.scalaclean.analysis

import java.io.File

abstract class CommonWriter(file: File) {
  var logger: ScopeLogging = _

  val writer = new SortedStringWriter(file.toPath)

  def endUnit(): Unit = {
    writer.flush()
  }

  def finish(): Unit = {
    writer.close()
  }

}
