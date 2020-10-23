package org.scalaclean.analysis

import java.io.File

//TODO use CommonWriter
class RelationshipsWriter(file: File) {

  var logger: ScopeLogging = _
  val writer               = new StringWriter(file.toPath)

  def commonOutput(from: HasModelCommon, token: String, to: HasModelCommon): String = {
    s"${from.newCsvString},$token,${to.newCsvString}"
  }

  def overrides(overrider: ModelMethod, overridden: HasModelCommon, isDirect: Boolean): Unit = {
    writeLine(
      overrider,
      overridden,
      s"${commonOutput(overrider, IoTokens.relOverrides, overridden)},$isDirect",
      s"${IoTokens.relOverrides} ${overridden.legacyCsvIDString}) Direct:$isDirect"
    )
  }

  def refers(container: HasModelCommon, target: HasModelCommon, isSynthetic: Boolean): Unit = {
    writeLine(
      container,
      target,
      s"${commonOutput(container, IoTokens.relRefers, target)},$isSynthetic",
      s"${IoTokens.relRefers} ${target.legacyCsvIDString}) Synthetic:$isSynthetic"
    )
  }

  def extendsCls(parentSym: HasModelCommon, childSym: ModelSymbol, direct: Boolean): Unit = {
    writeLine(
      parentSym,
      childSym,
      s"${commonOutput(childSym, IoTokens.relExtends, parentSym)},$direct",
      s"${IoTokens.relExtends} ${parentSym.legacyCsvIDString} Direct:$direct"
    )
  }

  def within(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    writeLine(
      outerSym,
      innerSym,
      s"${commonOutput(innerSym, IoTokens.relWithin, outerSym)}",
      s"${IoTokens.relWithin} ${outerSym.legacyCsvIDString}"
    )

  }

  def getterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(
      method,
      field,
      s"${commonOutput(method, IoTokens.relGetter, field)}",
      s"${IoTokens.relGetter} ${field.legacyCsvIDString}"
    )
  }

  def setterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(
      method,
      field,
      s"${commonOutput(method, IoTokens.relSetter, field)}",
      s"${IoTokens.relSetter} ${field.legacyCsvIDString}"
    )
  }
  def recordDuplicate(symbol: ModelSymbol) = {
    val msg = s"${symbol.idWithDeDuplicationSuffix},${IoTokens.duplicateOf},${symbol.newCsvString}"
    assert (writer.writeLine(msg))

    if (logger.debug)
      logger.scopeLog(s" -->[RELATIONSHIP] $msg")
    //always log duplicates
    logger.scopeLog(s" -->[SUMMARY] DUPLICATE!!! $msg")
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
