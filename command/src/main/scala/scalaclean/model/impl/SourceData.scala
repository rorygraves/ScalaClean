package scalaclean.model.impl

import java.nio.file.Path

import scalafix.v1.SemanticDocument

import scala.meta.Tree
import scala.reflect.ClassTag

/**
  * represents a source file, and has utilities to extract info from a SemanticDocument if needed
 *
  * @param project the containing project
  * @param path the relative path to the source from the project path
  */
case class SourceData(project: Project, path: Path) {
  lazy val doc: SemanticDocument = null //FixUtils.loadSemanticDoc(project, path)

  def treeAt[T <: Tree : ClassTag](start: Int, end:Int): T = {
    treesAt[T](start,end) match {
      case tree :: Nil => tree
      case Nil => throw new IllegalStateException("cant find tree")
      case t => throw new IllegalStateException("trees! $t")
    }
  }
  def treesAt[T <: Tree : ClassTag](start: Int, end:Int): List[T] = {
    val res = List.newBuilder[T]
    def traverse(tree: Tree): Unit = {
      val pos = tree.pos
      tree match {
        case t: T if pos.start == start && pos.end == end =>
          res += t
        case _ =>
      }
      if (pos.start <= start && pos.end >= end)
        tree.children foreach traverse
    }
    traverse(doc.tree)
    res.result()
  }
}
