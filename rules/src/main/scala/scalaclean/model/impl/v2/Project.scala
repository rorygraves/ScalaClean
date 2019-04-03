package scalaclean.model.impl.v2

import java.nio.file.{Path, Paths}
import java.util.concurrent.ConcurrentHashMap

import scalaclean.model.{Refers, Within}
import scalafix.v1.Symbol

import scala.collection.immutable

class Project(path: Path, val projects: Projects) {

  def read = ModelReader.read(this, path, projects.projectRoot)

  private val sourcesMap = new ConcurrentHashMap[String, SourceData]()
  def source(name:String) = {
    sourcesMap.computeIfAbsent(name, p => new SourceData(this, Paths.get(p)))
  }

}
class Projects(val projectRoot: Path, projectPaths: Path *) {
  val projects = projectPaths.toList map {new Project(_, this)}

  val (elements: Map[Symbol, ElementModelImpl], relsFrom)  = {
    val (elements, rels: immutable.Seq[BasicRelationshipInfo]) = projects.map(_.read) unzip

    val elementsMap = elements.flatten.toVector.map (e => e.symbol -> e) toMap

    val relsFrom = rels.reduce(_ + _)
    (elementsMap, relsFrom)
  }
  val relsTo = relsFrom.byTo
  relsFrom.complete(elements)

  elements.values foreach (_.complete)

}
