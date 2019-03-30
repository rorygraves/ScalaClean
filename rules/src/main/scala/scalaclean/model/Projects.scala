package scalaclean.model

import java.lang.ref.SoftReference
import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.inputs.Position
import scala.reflect.ClassTag

/**
  * all of the projects in an build.
  * This is the entry point to find symbols
  * @param projects the list of projects, each project is the result of a compilation
  */
class Projects(projects: Seq[Project]) {
}

/**
  * A Project is the result of a compilation of tree of scala files
  * it is aware of all of the symbols declared in a project
  * @param root
  * @param declaredSymbols
  */
class Project(root: Path, val declaredSymbols: Map[Symbol, SymbolPosition]) {
  //a cache of all of the documents
  private val docs = new ConcurrentHashMap[String, SemanticDocument]
}

object SymbolPosition {
  private val nopos = new SoftReference[Position](null)
}
class SymbolPosition(path: String, start:Int, end: Int) {
  private var _pos: SoftReference[Position] = SymbolPosition.nopos
}