package org.scalaclean.analysis

import java.util.concurrent.atomic.AtomicInteger

import scalaclean.model.ElementId
import scalaclean.model.impl.FieldPathImpl

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.Global

trait ModelSymbolBuilder extends SemanticdbOps {
  val global: scala.tools.nsc.Global

  def debug: Boolean
  def sourceDirs: List[String]

  private val symbolNames = mutable.Map[global.Symbol, String]()
  private val ownerSymbolNames = mutable.Map[global.Symbol, LocalSymbolNames]()
  class LocalSymbolNames {
    val localSymbolNames: mutable.Map[global.Symbol, String] = mutable.Map[global.Symbol, String]()
    val idGen: mutable.Map[String, AtomicInteger] = mutable.Map[String, AtomicInteger]()
    val globalIdGen: AtomicInteger = new AtomicInteger
  }

  private def localId(sym: global.Symbol) = {
    val local = ownerSymbolNames.getOrElseUpdate(sym.owner, new LocalSymbolNames)

    local.localSymbolNames.getOrElseUpdate(sym,
      if (sym.pos.isDefined) {
        val pos = s"${sym.pos.line}/${sym.pos.column}"
        val suffix = local.idGen.getOrElseUpdate(pos, new AtomicInteger).incrementAndGet()
        s"{{Local-Pos[$pos]#$suffix}}"
      } else
        s"{{Local-NoPos#${local.globalIdGen.incrementAndGet()}}}")
  }

  private def localName(sym: global.Symbol): String = sym match {
    case sym if sym.isLocalToBlock =>
      //for locals we don't have to preserve identity across compiles as they can't be referenced
      //but we need to preserve across the same compile!
      localId(sym) + suffix(sym)
    case sym if sym.isMethod =>
      "{M}" + sym.encodedName +
        typeParams(sym) +
        sym.paramss.map { params => params.map(param => paramName(param)).mkString(";") }.mkString("(", ")(", ")") +
        suffix(sym)
    case sym => sym.encodedName + suffix(sym)
  }

  private def suffix(sym: global.Symbol) = sym match {
    case _: global.ModuleClassSymbol => ""
    case _: global.TermSymbol        => "."
    case _: global.TypeSymbol        => "#"
    case _: global.NoSymbol          => ""
    case _ => throw new AssertionError(s"Symbols should be either terms, types, or NoSymbol, but got $sym")
  }

  private def paramName(param: global.Symbol) = param.info.typeSymbol.fullName + typeParams(param)

  private def typeParams(sym: global.Symbol) =
    if (sym.typeParams.isEmpty) ""
    else sym.typeParams.map(_.info.typeSymbol.fullName).mkString("[", ";", "]")

  private def fullNameString(sym: global.Symbol): String = {
    def recur(sym: global.Symbol): String = {
      if (sym.isRootSymbol || sym == global.NoSymbol) sym.nameString
      else recur(sym.owner) + "/" + localName(sym)
    }

    recur(sym)
  }

  private val mSymbolCache2 = mutable.Map[global.Symbol, ModelCommon]()
  private val mSymbolCache = mutable.Map[global.Symbol, ModelCommon]()

  def externalSymbol(gSym: Global#Symbol): ModelCommon = {
    asMSymbol(gSym.asInstanceOf[global.Symbol])
  }

  def asMSymbol(gSym: global.Symbol): ModelCommon = {
    asMSymbolX(gSym, forceField = false)
  }
  def asMSymbolForceField(gSym: global.Symbol): ModelCommon = {
    val unforced = asMSymbolX(gSym, forceField = false)
    if (unforced.elementId.isInstanceOf[FieldPathImpl]) unforced
    else asMSymbolX(gSym, forceField = true)
  }
  private def newIsGlobal(gSym: global.Symbol, expected: Boolean): Boolean  = {
    def check(calculated: Boolean) = {
      if (calculated != expected) {
        println("************************************")
      }
      calculated
    }
    @tailrec def determineGlobal(sym: global.Symbol): Boolean = {
      if (sym.hasPackageFlag)
        check(true)
      else if (sym == g.NoSymbol)
        check(false)
      else if ((sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter)
        check(false)
      else if (sym.owner.thisSym == sym)
        check(false)
      else if (sym.isLocalDummy)
        check(false)
      else if (sym.isRefinementClass)
        check(false)
      else if (sym.isAnonymousClass)
        check(false)
      else if (sym.isAnonymousFunction)
        check(false)
      else if (sym.isExistential)
        check(false)
      else
        determineGlobal(sym.owner)
    }
    if (gSym.owner.isTerm)
      check(false)
    else determineGlobal(gSym)

  }
  private def asMSymbolX(gSym: global.Symbol, forceField:Boolean): ModelCommon = {
    val cache = if (forceField) mSymbolCache2 else mSymbolCache

    cache.getOrElseUpdate(gSym, {
      val isGlobal = gSym.isSemanticdbGlobal && !gSym.isLocalToBlock
      val newGlobal = newIsGlobal(gSym, isGlobal)
      println(s"GGGGGGGGGGGGGGGGGGG:isGlobal/newIsGlobal = $isGlobal/$newGlobal   ---   $gSym")
      val (startPos, endPos, focusPos) = if (gSym.pos == NoPosition) (-1, -1, -1) else (gSym.pos.start, gSym.pos.end, gSym.pos.focus.start)
      val sourceFile = if (gSym.sourceFile != null)
        mungeUnitPath(gSym.sourceFile.toString)
      else
        "-"

      val newName = if (forceField) ElementId.applyAndForceField(gSym) else ElementId(gSym) //getNewName(gSym) + specialSuffix

      ModelCommon(isGlobal, newName, sourceFile, startPos, endPos, focusPos, gSym.nameString)

    }
    )
  }

  // TODO this is a total hack - need to discover the source root of the compilation unit and remove
  def mungeUnitPath(input: String): String = {
    val sourceDirForInput = sourceDirs.find(input.startsWith).getOrElse(throw new IllegalArgumentException(s"Missing src dir for $input"))
    val idx = input.indexOf(sourceDirForInput) + sourceDirForInput.length + 1
    input.substring(idx)
  }

}
