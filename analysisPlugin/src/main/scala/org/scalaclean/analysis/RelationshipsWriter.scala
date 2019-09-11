package org.scalaclean.analysis

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.tools.nsc.Global

class RelationshipsWriter(file: File, val global: Global) extends  SemanticdbOps {
  def extendsCls(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    pr.println(s"${innerSym.csvString},${IoTokens.relExtends},${outerSym.csvString}")
  }


  val  pr = new PrintWriter(new BufferedWriter(new FileWriter(file)))

  def within(outerSym: ModelSymbol, innerSym: ModelSymbol): Unit = {
    pr.println(s"${innerSym.csvString},${IoTokens.relWithin},${outerSym.csvString}")
  }


  def finish(): Unit = {
    pr.close()
  }

}
