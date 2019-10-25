package org.scalaclean.analysis

import java.io.File

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.tools.nsc.Global

class ExtensionWriter(file: File, val global: Global) extends SemanticdbOps {
  def writeExtensions(mSymbol: ModelSymbol, data: Seq[ExtensionData]): Unit = {
    if (data.nonEmpty) {
      val prefix = s"${mSymbol.csvString},${mSymbol.newCsvString},"
      data foreach {
        d =>
          writeLine(s"$prefix,${d.getClass.getName},${d.toCsv}", d)
      }
    }
  }


  var logger: ScopeLogging = _
  val writer = new SortedStringWriter(file.toPath)


  def endUnit(): Unit = {
    writer.flush()
  }

  def writeLine(msg: String, summary: Product): Unit = {
    if (!writer.writeLine(msg))
      logger.scopeLog(s" -->[DUPLICATE-EXT] $msg")

    if (logger.debug) {
      logger.scopeLog(s" -->[EXT] $msg")
      logger.scopeLog(s" -->[SUMMARY] $summary")
    }
  }

  def finish(): Unit = {
    writer.close()
  }

}
