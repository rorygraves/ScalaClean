package org.scalaclean.analysis

import java.io.File

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.tools.nsc.Global

class RelationshipsWriter(file: File, val global: Global) extends  SemanticdbOps {

  var logger: ScopeLogging = _
  val writer = new SortedStringWriter(file.toPath)

  def overrides(overrider: ModelMethod, overridden: HasModelCommon, isDirect: Boolean): Unit = {
    writeLine(s"${overrider.csvString},${IoTokens.relOverrides},${overridden.csvString},$isDirect",
      s"${IoTokens.relOverrides} ${overridden.csvString}) Direct:$isDirect")
  }

  def refers(container: HasModelCommon, target: HasModelCommon, isSynthetic: Boolean): Unit = {
    writeLine(s"${container.csvString},${IoTokens.relRefers},${target.csvString},$isSynthetic",
      s"${IoTokens.relRefers} ${target.csvString}) Synthetic:$isSynthetic")
  }

  def extendsCls(parentSym: HasModelCommon, childSym: ModelSymbol, direct: Boolean): Unit = {
    writeLine(s"${childSym.csvString},${IoTokens.relExtends},${parentSym.csvString},$direct",
      s"${IoTokens.relExtends} ${parentSym.csvString} Direct:$direct")
  }

  def within(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    writeLine(s"${innerSym.csvString},${IoTokens.relWithin},${outerSym.csvString}",
      s"${IoTokens.relWithin} ${outerSym.csvString}")

  }

  def getterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(s"${method.csvString},${IoTokens.relGetter},${field.csvString}",
      s"${IoTokens.relGetter} ${field.csvString}")
  }

  def setterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(s"${method.csvString},${IoTokens.relSetter},${field.csvString}",
      s"${IoTokens.relSetter} ${field.csvString}")
  }

  def endUnit(): Unit = {
    writer.flush()
  }
  def writeLine(msg: String, summary: String): Unit = {
    if (! writer.writeLine(msg))
      logger.scopeLog(s" -->[DUPLICATE] $msg")

    if (logger.debug) {
      logger.scopeLog(s" -->[RELATIONSHIP] $msg")
      logger.scopeLog(s" -->[SUMMARY] $summary")
    }

  }

  def finish(): Unit = {
    writer.close()
  }

}
