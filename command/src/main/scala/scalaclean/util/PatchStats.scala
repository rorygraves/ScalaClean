package scalaclean.util

import java.nio.file.Path

class PatchStats {

  def printSummary(projectName: String): Unit = {
    println(s"""Files           Observed = $filesVisited (Skipped = ${_skipped})
               |Elements        Observed = $elementsVisited
               |Source Elements Observed = $sourceElementsVisited
               |Elements        Changed  = $elementsChanged
               |Effect rate              = ${(elementsChanged.toDouble / sourceElementsVisited.toDouble * 10000).toInt / 100} %
               |""".stripMargin)

  }

  private var _filesVisited          = 0
  private var _skipped               = 0
  private var _elementsVisited       = 0
  private var _sourceElementsVisited = 0
  private var _elementsChanged       = 0

  def addFrom(patcher: ScalaCleanTreePatcher): Unit = {
    _filesVisited += patcher.filesVisited
    _elementsVisited += patcher.elementsVisited
    _sourceElementsVisited += patcher.sourceElementsVisited
    _elementsChanged += patcher.elementsChanged
  }
  def skippedFile(filename: Path) = {
    _skipped += 1
  }

  def filesVisited          = _filesVisited
  def elementsVisited       = _elementsVisited
  def sourceElementsVisited = _sourceElementsVisited
  def elementsChanged       = _elementsChanged

}
