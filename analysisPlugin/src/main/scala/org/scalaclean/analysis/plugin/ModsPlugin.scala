package org.scalaclean.analysis.plugin

import org.scalaclean.analysis._
import scalaclean.model.ElementId

import scala.reflect.internal.Flags

object ModsPlugin extends ExtensionPluginFactory {
  override def create(sc: ScalaCompilerPluginComponent, paramIgnored: String) = new ModsPlugin(sc)
}

class ModsPlugin(val sc: ScalaCompilerPluginComponent) extends ExtensionPlugin {

  override def extendedData(
      mSymbol: ModelSymbol,
      tree: Option[g.Tree],
      _symbol: g.Symbol,
      enclosingModel: List[ModelSymbol]
  ): List[ExtensionData] = {
    // Visibility encoding seems to be like this
    // for vals and vars the encoding is on the getter (and setter for vars) not on the val & var
    // as the actual field is private[this]
    // Mods dont have the info that we need after typer, so we take it from the symbol
    // private[this]   === Flags.PRIVATE | Flags.LOCAL
    // private         === Flags.PRIVATE
    // private[foo]    === no flags but 'privateWithin=="foo"'
    // protected[this] === Flags.PROTECTED | Flags.LOCAL
    // protected       === Flags.PROTECTED
    // protected[foo]  === Flags.PROTECTED &  'privateWithin=="foo"'
    val vis: List[VisibilityData] = {
      val symbol: g.Symbol = mSymbol match {
        case field: ModelField             => _symbol.getterIn(_symbol.owner)
        case accessor: ModelAccessorMethod => g.NoSymbol
        case fields: ModelFields           => g.NoSymbol
        case _                             => _symbol
      }
      if (symbol.hasFlag(Flags.PROTECTED | Flags.PRIVATE) || symbol.hasAccessBoundary) {
        val within =
          if (symbol.hasAccessBoundary) Some(sc.externalSymbol(symbol.privateWithin).elementId)
          else if (symbol.hasFlag(Flags.LOCAL)) Some(sc.elementIds.childThis(enclosingModel.head.common.elementId))
          else None
        val group = if (symbol.hasFlag(Flags.PROTECTED)) "protected" else "private"
        symbol.sourceFile
        List(VisibilityData(Int.MinValue, Int.MinValue, group, within))
      } else Nil
    }
    val others: List[ExtensionData] = tree match {
      case Some(memberDef: g.MemberDefApi) =>
        memberDef.mods.positions.collect { case (f, pos) =>
          val basePos = memberDef.pos.start
          ModData(pos.start - basePos, pos.end - basePos, f)
        }(scala.collection.breakOut)
      case _ => Nil
    }
    vis ::: others

  }

}
