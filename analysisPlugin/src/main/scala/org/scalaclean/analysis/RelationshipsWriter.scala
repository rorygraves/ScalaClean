package org.scalaclean.analysis

import java.io.File

//TODO use CommonWriter
class RelationshipsWriter(file: File) {

  var logger: ScopeLogging = _
  val writer               = new StringWriter(file.toPath)

  def commonOutput(from: HasModelCommon, token: String, to: HasModelCommon): String = {
    s"${from.newCsvString},$token,${to.newCsvString}"
  }

  def overrides(overrider: ModelMethod, overridden: HasModelCommon, isDirect: Boolean, synthetic: Boolean): Unit = {
    writeLine(
      overrider,
      overridden,
      s"${commonOutput(overrider, IoTokens.relOverrides, overridden)},$isDirect,$synthetic",
      s"${IoTokens.relOverrides}) Direct:$isDirect, synthetic:$synthetic"
    )
  }

  def refers(container: HasModelCommon, target: HasModelCommon, isSynthetic: Boolean): Unit = {
    writeLine(
      container,
      target,
      s"${commonOutput(container, IoTokens.relRefers, target)},$isSynthetic",
      s"${IoTokens.relRefers}) Synthetic:$isSynthetic"
    )
  }

  def extendsCls(parentSym: HasModelCommon, childSym: ModelSymbol, direct: Boolean): Unit = {
    writeLine(
      parentSym,
      childSym,
      s"${commonOutput(childSym, IoTokens.relExtends, parentSym)},$direct",
      s"${IoTokens.relExtends} Direct:$direct"
    )
  }

  def within(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    writeLine(
      outerSym,
      innerSym,
      s"${commonOutput(innerSym, IoTokens.relWithin, outerSym)}",
      s"${IoTokens.relWithin}"
    )

  }

  def getterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(
      method,
      field,
      s"${commonOutput(method, IoTokens.relGetter, field)}",
      s"${IoTokens.relGetter} $method $field"
    )
  }

  def setterFor(method: ModelCommon, field: ModelCommon): Unit = {
    writeLine(
      method,
      field,
      s"${commonOutput(method, IoTokens.relSetter, field)}",
      s"${IoTokens.relSetter} $method $field"
    )
  }
  def relatedCtorParam(field: ModelField, param: ModelCommon): Unit = {
    writeLine(
      field,
      param,
      s"${commonOutput(field, IoTokens.ctorParam, param)}",
      s"${IoTokens.ctorParam} $field $param"
    )
  }
  def defaultGetterMethod(field: ModelField, defaultGetter: ModelCommon): Unit = {
    writeLine(
      field,
      defaultGetter,
      s"${commonOutput(field, IoTokens.defaultGetter, defaultGetter)}",
      s"${IoTokens.defaultGetter} $field $defaultGetter"
    )
  }
  def recordSelfTypeField(cls: ClassLike, selfTypeField: ModelField): Unit = {
    writeLine(
      cls,
      selfTypeField,
      s"${commonOutput(cls, IoTokens.selfType, selfTypeField)}",
      s"${IoTokens.selfType} $cls $selfTypeField"
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
