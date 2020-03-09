package org.scalaclean.analysis

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.reflect.internal.util.NoPosition

trait ModelSymbolBuilder extends SemanticdbOps {
  val global: scala.tools.nsc.Global

  def debug: Boolean

  private val symbolNames = mutable.Map[global.Symbol, String]()
  private val localSymbolNames = mutable.Map[global.Symbol, String]()
  private val idGen = new AtomicInteger()

  private def localId(sym: global.Symbol) = {
    localSymbolNames.getOrElseUpdate(sym, s"{{Local#${idGen.incrementAndGet()}}}")
  }

  private def localName(sym: global.Symbol): String = sym match {
    case sym if sym.isLocalToBlock =>
      //for locals we dont have to preserve identity across compiles asthey ant be referenced
      //but we need to preserve across the same compile!
      localId(sym)
    case sym if sym.isMethod =>
      "{M}"+sym.encodedName +
        sym.disambiguator +
        (if (sym.typeParams.isEmpty) ""
        else "[" + sym.typeParams.map { param => param.info.typeSymbol.fullName }.mkString(";") + "]"
          ) +
        sym.paramss.map { params => params.map(param => paramName(param)).mkString(";") }.mkString("(", "", ")") +
        sym.asMethod.returnType.toString.replace(',', ';') // no commas in a CSV-bounded world
    case sym if sym.isModule => sym.encodedName + "#"
    case sym if sym.isModuleOrModuleClass => sym.encodedName + "@"
    case sym if sym.isClass => sym.encodedName + "."
    case sym => sym.encodedName
  }
  private def paramName(param: global.Symbol) = {
    param.info.typeSymbol.fullName +
      (if (param.typeParams.isEmpty) ""
      else "[" + param.typeParams.map { param => param.info.typeSymbol.fullName }.mkString(";") + "]"
        )
  }

  private def fullNameString(sym: global.Symbol): String = {
    def recur(sym: global.Symbol): String = {
      if (sym.isRootSymbol || sym == global.NoSymbol) sym.nameString
      else recur(sym.owner) + "/" + localName(sym)
    }

    recur(sym)
  }

  private def getNewName(sym: global.Symbol): String = {
    //    if (debug)
    //      println(s"getNewName $sym")
    symbolNames.getOrElseUpdate(sym, fullNameString(sym))
    //    import global.{ModuleDef, ClassDef, ValDef, DefDef, NoSymbol}
    //
    //    symbolNames.getOrElseUpdate(sym, sym =>
    //    val usefulParent = sym.ownersIterator.collectFirst {
    //      case defn: ModuleDef => getNewName(defn.info.termSymbol)
    //      case defn: ClassDef =>  getNewName(defn.info.termSymbol)
    //      case defn: ValDef =>  getNewName(defn.info.termSymbol)
    //      case defn: DefDef =>  getNewName(defn.info.termSymbol)
    //    }.getOrElse("")
    //      s"$usefulParent/${sym.nameString}".reverse.mkString("/")
    //
    //  )
  }

  private val mSymbolCache2 = mutable.Map[global.Symbol, ModelCommon]()
  private val mSymbolCache = mutable.Map[global.Symbol, ModelCommon]()

  def asMSymbol(gSym: global.Symbol, special:Boolean = false): ModelCommon = {
    val cache = if (special) mSymbolCache2 else mSymbolCache

    cache.getOrElseUpdate(gSym, {
      val isGlobal = gSym.isSemanticdbGlobal && !gSym.isLocalToBlock
      val newIsGlobal = gSym.ownersIterator.forall(o => o.isType && !o.isSynthetic)
      val sString = gSym.toSemantic
      val (startPos, endPos) = if (gSym.pos == NoPosition) (-1, -1) else (gSym.pos.start, gSym.pos.end)
      val sourceFile = if (gSym.sourceFile != null)
        mungeUnitPath(gSym.sourceFile.toString)
      else
        "-"
      val specialSuffix = if (special) "~" else ""
      val name = gSym.nameString + specialSuffix

      val newName = getNewName(gSym) + specialSuffix

      ModelCommon(isGlobal, sString, newName, sourceFile, startPos, endPos, name)

    }
    )
  }

  // TODO this is a total hack - need to discover the source root of the compilation unit and remove
  def mungeUnitPath(input: String): String = {
    val idx = input.indexOf("src/main/scala")
    if (idx != -1)
      input.substring(idx + "src/main/scala".length + 1)
    else input
  }

}
