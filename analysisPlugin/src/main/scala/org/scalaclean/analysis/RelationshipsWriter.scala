package org.scalaclean.analysis

import java.io.File

import scala.tools.nsc.Global

class RelationshipsWriter(file: File, val global: Global) {

  var logger: ScopeLogging = _
  val writer = new StringWriter(file.toPath)

  def commonOutput(from: HasModelCommon, token: String, to: HasModelCommon): String = {
    s"${from.csvString},${from.newCsvString},$token,${to.csvString},${to.newCsvString}"
  }

  def overrides(overrider: ModelMethod, overridden: HasModelCommon, isDirect: Boolean): Unit = {
    writeLine(overrider, overridden, s"${commonOutput(overrider, IoTokens.relOverrides, overridden)},$isDirect",
      s"${IoTokens.relOverrides} ${overridden.csvString}) Direct:$isDirect")
  }

  def refers(container: HasModelCommon, target: HasModelCommon, isSynthetic: Boolean): Unit = {
    writeLine(container, target, s"${commonOutput(container, IoTokens.relRefers, target)},$isSynthetic",
      s"${IoTokens.relRefers} ${target.csvString}) Synthetic:$isSynthetic")
  }

  def extendsCls(parentSym: HasModelCommon, childSym: ModelSymbol, direct: Boolean): Unit = {
    writeLine(parentSym, childSym, s"${commonOutput(childSym, IoTokens.relExtends, parentSym)},$direct",
      s"${IoTokens.relExtends} ${parentSym.csvString} Direct:$direct")
  }

  def within(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    writeLine(outerSym, innerSym, s"${commonOutput(innerSym, IoTokens.relWithin, outerSym)}",
      s"${IoTokens.relWithin} ${outerSym.csvString}")

  }

  def getterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(method, field, s"${commonOutput(method, IoTokens.relGetter, field)}",
      s"${IoTokens.relGetter} ${field.csvString}")
  }

  def setterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(method, field, s"${commonOutput(method, IoTokens.relSetter, field)}",
      s"${IoTokens.relSetter} ${field.csvString}")
  }

  def endUnit(): Unit = {
    writer.flush()
  }

  def writeLine(source: HasModelCommon, destination: HasModelCommon, msg: String, summary: String): Unit = {
    if (source.common == destination.common) {
      logger.scopeLog(s" -->[IGNORED-REL] (source == dest) $msg")
    } else {
      if (!writer.writeLine(msg))
        logger.scopeLog(s" -->[DUPLICATE-REL] $msg")

      if (logger.debug) {
        logger.scopeLog(s" -->[RELATIONSHIP] $msg")
        logger.scopeLog(s" -->[SUMMARY] $summary")
      }
    }
  }

  def finish(): Unit = {
    writer.close()
  }

}
