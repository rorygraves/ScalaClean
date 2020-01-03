package scalaclean.model

import scalafix.v1.Patch
import scalapb.descriptors.ScalaType.Message

import scala.meta.{Pat, Stat, Tree}

object Utils {
  def addMarker2(stat: Stat, message: String): Option[SCPatch] =
    Some(SCPatch(stat.tokens.head.start, stat.tokens.head.start, s"/* *** SCALA CLEAN $message */"))

  def addMarker(stat: Stat, message: String): Patch =
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
    visitor.res
  }
}
