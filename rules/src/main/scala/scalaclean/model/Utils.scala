package scalaclean.model

import scala.meta.{Pat, Tree}

object Utils {
  def readVars(pats: List[Pat]): List[Pat.Var] = {
    object visitor {
      var res = List.empty[Pat.Var]

      def visitTree(tree: Tree): Unit = {
        tree match {
          case field: Pat.Var =>
            res ::= field
          case _ =>
        }
        tree.children.foreach {visitTree(_)}
      }
    }
    pats foreach visitor.visitTree
    assert (visitor.res.nonEmpty)
    visitor.res
  }
}
