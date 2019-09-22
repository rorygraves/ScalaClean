package org.scalaclean.analysis

import java.io.File

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.tools.nsc.Global

class RelationshipsWriter(file: File, val global: Global) extends  SemanticdbOps {
  val writer = new SortedStringWriter(file.toPath)

  def overrides(overrider: ModelSymbol, overridden: ModelSymbol, isDirect: Boolean): Unit = {
    writer.writeLine(s"${overrider.csvString},${IoTokens.relOverrides},${overridden.csvString},$isDirect")
  }

  def refers(container: ModelSymbol, target: ModelSymbol,isSynthetic: Boolean) : Unit = {
    writer.writeLine(s"${container.csvString},${IoTokens.relRefers},${target.csvString},$isSynthetic")
  }

  def extendsCls(outerSym: ModelSymbol, innerSym: ModelSymbol, direct: Boolean): Unit = {
    writer.writeLine(s"${innerSym.csvString},${IoTokens.relExtends},${outerSym.csvString},$direct")
  }

  def within(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    writer.writeLine(s"${innerSym.csvString},${IoTokens.relWithin},${outerSym.csvString}")
  }


  def finish(): Unit = {
    writer.close()
  }

}
