package scalaclean.model

import scalafix.v1.Patch

import scala.meta.{Pat, Stat, Tree}

object Utils {
  def addError(stat: Stat, message: String) =
    Patch.addLeft(stat.tokens.head, s"/* *** SCALA CLEAN ERROR $message */")

  def addMarker(stat: Stat, message: String) =
    Patch.addLeft(stat.tokens.head, s"/* *** SCALA CLEAN $message */")

  def readVars(pats: List[Pat]): List[Pat.Var] = {
    object visitor {
      var res = List.empty[Pat.Var]

      def visitTree(tree: Tree): Unit = {
        tree match {
          case field: Pat.Var =>
            res ::= field
          case _ =>
        }
        tree.children.foreach {
          visitTree
        }
      }
    }
    pats foreach visitor.visitTree
    assert(visitor.res.nonEmpty)
    visitor.res
  }
}
