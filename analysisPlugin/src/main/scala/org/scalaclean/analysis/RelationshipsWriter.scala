package org.scalaclean.analysis

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.tools.nsc.Global

class RelationshipsWriter(file: File, val global: Global) extends  SemanticdbOps {
  val  pr = new PrintWriter(new BufferedWriter(new FileWriter(file)))

  def overrides(overrider: ModelSymbol, overridden: ModelSymbol, isDirect: Boolean): Unit = {
    pr.println(s"${overrider.csvString},${IoTokens.relOverrides},${overridden.csvString},$isDirect")
  }

  def refers(container: ModelSymbol, target: ModelSymbol,isSynthetic: Boolean) : Unit = {
    pr.println(s"${container.csvString},${IoTokens.relRefers},${target.csvString},$isSynthetic")
  }

  def extendsCls(outerSym: ModelSymbol, innerSym: ModelSymbol, direct: Boolean): Unit = {
    pr.println(s"${innerSym.csvString},${IoTokens.relExtends},${outerSym.csvString},$direct")
  }

  def within(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    pr.println(s"${innerSym.csvString},${IoTokens.relWithin},${outerSym.csvString}")
  }


  def finish(): Unit = {
    pr.close()
  }

}
