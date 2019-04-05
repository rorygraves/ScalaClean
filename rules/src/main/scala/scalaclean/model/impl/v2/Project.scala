package scalaclean.model.impl.v2

import java.nio.file.{Path, Paths}
import java.util.concurrent.ConcurrentHashMap

import scalaclean.model.{Refers, Within}
import scalafix.v1.{Symbol, SymbolInformation}

import scala.collection.immutable

class Project(path: Path, val projects: Projects) {
  def symbolInfo(symbol: Symbol): SymbolInformation = ???

  def read = ModelReader.read(this, path, projects.projectRoot)

  private val sourcesMap = new ConcurrentHashMap[String, SourceData]()
  def source(name:String) = {
    sourcesMap.computeIfAbsent(name, p => new SourceData(this, Paths.get(p)))
  }

}
class Projects(val projectRoot: Path, projectPaths: Path *) {
  val projects = projectPaths.toList map {new Project(_, this)}

  val elements: Map[Symbol, ElementModelImpl] = {
    val (elements, rels: immutable.Seq[BasicRelationshipInfo]) = projects.map(_.read) unzip

    val elementsMap = elements.flatten.toVector.map (e => e.symbol -> e) toMap

    val relsFrom = rels.reduce(_ + _)
    val relsTo = relsFrom.byTo

    relsFrom.complete(elementsMap)
    elementsMap.values foreach (_.complete(elementsMap, relsFrom, relsTo))

    elementsMap
  }


  private val infos = new ConcurrentHashMap[Symbol, SymbolInformation]()
  def info(sym:Symbol) = {
    infos.computeIfAbsent(sym, p => ???)
  }

}
