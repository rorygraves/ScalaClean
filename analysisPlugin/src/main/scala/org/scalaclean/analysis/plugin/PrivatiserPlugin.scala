package org.scalaclean.analysis.plugin
import org.scalaclean.analysis.{ExtensionData, ModelSymbol, ScalaCompilerPluginComponent, VisibilityData}

import scala.reflect.internal.Flags

object PrivatiserPlugin extends ExtensionPluginFactory {
  override def create(sc: ScalaCompilerPluginComponent, paramIgnored: String)= new PrivatiserPlugin(sc)
}
class PrivatiserPlugin(val sc: ScalaCompilerPluginComponent) extends ExtensionPlugin {
  override def extendedData(mSymbol: ModelSymbol, tree: g.Tree): List[ExtensionData] = {
    tree match {
      case d : g.MemberDefApi if d.mods.hasFlag(Flags.PROTECTED | Flags.PRIVATE) =>
        val within = if (d.mods.hasAccessBoundary) d.mods.accessString else if (d.mods.hasFlag(Flags.LOCAL)) "this" else ""
        val (flag, vis)  = if (d.mods.hasFlag(Flags.PROTECTED)) (Flags.PROTECTED, "protected") else (Flags.PRIVATE, "private")
        d.mods.positions.get(flag) match {
          case Some(pos) =>
            val basePos = tree.pos.start
            List(VisibilityData(pos.start - basePos,pos.end - basePos, vis, within))
          case None =>
            List(VisibilityData(0, 0, vis, within))
        }
      case _ => Nil
    }
  }
}
