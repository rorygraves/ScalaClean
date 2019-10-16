package org.scalaclean.analysis

import scala.collection.mutable
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.reflect.internal.util.NoPosition

trait ModelSymbolBuilder extends SemanticdbOps {

  private val mSymbolCache = mutable.Map[global.Symbol, ModelCommon]()
  def asMSymbol(gSym: global.Symbol): ModelCommon = {

    mSymbolCache.getOrElseUpdate(gSym,{
      val isGlobal = gSym.isSemanticdbGlobal
      val sString = gSym.toSemantic
      val (startPos,endPos) = if(gSym.pos == NoPosition) (-1,-1) else (gSym.pos.start, gSym.pos.end)
      val sourceFile = if(gSym.sourceFile != null)
        mungeUnitPath(gSym.sourceFile.toString)
      else
        "-"
      val name = gSym.nameString
      ModelCommon(isGlobal, sString, sourceFile, startPos, endPos, name)

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
