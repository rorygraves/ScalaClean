package org.scalaclean.analysis

import scalaclean.model.{ElementIdManager, FieldPath}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.Global

trait ModelSymbolBuilder {
  val global: scala.tools.nsc.Global

  def debug: Boolean
  def sourceDirs: List[String]
  val elementIds = new ElementIdManager

  private val mSymbolCache2 = mutable.Map[global.Symbol, ModelCommon]()
  private val mSymbolCache  = mutable.Map[global.Symbol, ModelCommon]()
  private val aliases  = mutable.Map[global.Symbol, ModelCommon]()

  def externalSymbol(gSym: Global#Symbol): ModelCommon = {
    asMSymbol(gSym.asInstanceOf[global.Symbol])
  }

  def addAlias(alias: global.Symbol, model: ModelSymbol): Unit = {
    assert(!aliases.contains(alias))
    assert(!mSymbolCache.contains(alias))
    assert(model.tree.symbol != alias)
    aliases(alias) = model.common
    mSymbolCache(alias) = model.common
  }
  def removeAlias(alias: global.Symbol): Unit = {
    assert(aliases.contains(alias))
    aliases -= alias
    mSymbolCache -= alias
  }
  def asMSymbol(gSym: global.Symbol): ModelCommon = {
    asMSymbolX(gSym, forceField = false)
  }

  def asMSymbolForceField(gSym: global.Symbol): ModelCommon = {
    val unforced = asMSymbolX(gSym, forceField = false)
    if (unforced.elementId.pathType == FieldPath) unforced
    else asMSymbolX(gSym, forceField = true)
  }

  private def newIsGlobal(gSym: global.Symbol): Boolean = {
    @tailrec def determineGlobal(sym: global.Symbol): Boolean = {
      if (sym.hasPackageFlag)
        true
      else if (
        sym == global.NoSymbol
        || ((sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter)
        || sym.owner.thisSym == sym
        || sym.isLocalDummy
        || sym.isRefinementClass
        || sym.isAnonymousClass
        || sym.isAnonymousFunction
        || sym.isExistential
      )
        false
      else
        determineGlobal(sym.owner)
    }

    if (gSym.owner.isTerm)
      false
    else determineGlobal(gSym)

  }

  private def asMSymbolX(gSym: global.Symbol, forceField: Boolean): ModelCommon = {
    val cache = if (forceField) mSymbolCache2 else mSymbolCache

    cache.getOrElseUpdate(
      gSym, {
        val isGlobal = newIsGlobal(gSym)
        val (startPos, endPos, focusPos) =
          if (gSym.pos == NoPosition) (-1, -1, -1) else (gSym.pos.start, gSym.pos.end, gSym.pos.focus.start)
        val sourceFile =
          if (gSym.sourceFile != null)
            mungeUnitPath(gSym.sourceFile.toString)
          else
            "-"

        val newName =
          if (forceField) elementIds.applyAndForceField(gSym) else elementIds(gSym) //getNewName(gSym) + specialSuffix

        ModelCommon(isGlobal, newName, sourceFile, startPos, endPos, focusPos, gSym.nameString)

      }
    )
  }

  // TODO this is a total hack - need to discover the source root of the compilation unit and remove
  def mungeUnitPath(input: String): String = {
    val sourceDirForInput =
      sourceDirs.find(input.startsWith).getOrElse(throw new IllegalArgumentException(s"Missing src dir for $input"))
    val idx = input.indexOf(sourceDirForInput) + sourceDirForInput.length + 1
    input.substring(idx)
  }

}
