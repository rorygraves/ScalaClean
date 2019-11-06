package org.scalaclean.analysis.plugin

import org.scalaclean.analysis.{ExtensionData, ModelSymbol, ScalaCompilerPluginComponent}

import scala.reflect.internal.Flags

object ModsPlugin extends ExtensionPluginFactory {
  override def create(sc: ScalaCompilerPluginComponent, paramIgnored: String) = new ModsPlugin(sc)
}

class ModsPlugin(val sc: ScalaCompilerPluginComponent) extends ExtensionPlugin {
  override def extendedData(mSymbol: ModelSymbol, tree: g.Tree): List[ExtensionData] = {
    tree match {
      case d: g.MemberDefApi =>
        val vis: List[VisibilityData] = if (d.mods.hasFlag(Flags.PROTECTED | Flags.PRIVATE)) {
          val within = if (d.mods.hasAccessBoundary) d.mods.accessString else if (d.mods.hasFlag(Flags.LOCAL)) "this" else ""
          val (flag, vis) = if (d.mods.hasFlag(Flags.PROTECTED)) (Flags.PROTECTED, "protected") else (Flags.PRIVATE, "private")
          d.mods.positions.get(flag) match {
            case Some(pos) =>
              val basePos = tree.pos.start
              List(VisibilityData(pos.start - basePos, pos.end - basePos, vis, within))
            case None =>
              List(VisibilityData(Int.MinValue, Int.MinValue, vis, within))
          }
        } else Nil
        val others: List[ExtensionData] = d.mods.positions.flatMap {
          case (Flags.PROTECTED | Flags.PRIVATE, _) => Nil
          case (f, pos) =>
            val basePos = tree.pos.start
            List(ModData(pos.start - basePos, pos.end - basePos, f, Flags.flagToString(f)))
        }(collection.breakOut)
        vis ::: others
      case _ => Nil
    }
  }
}
