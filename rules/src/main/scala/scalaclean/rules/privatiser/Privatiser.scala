package scalaclean.rules.privatiser

import scalaclean.model.{ModelElement, ObjectModel}
import scalaclean.rules.AbstractRule
import scalaclean.util.{Scope, SymbolTreeVisitor, SymbolUtils, TokenHelper}
import scalafix.patch.Patch
import scalafix.v1.SemanticDocument

import scala.meta.tokens.Token
import scala.meta.{Import, Mod, Pat, Stat}

class Privatiser extends AbstractRule("Privatiser") {
  import SymbolUtils._
  type Colour = PrivatiserLevel

  override def markInitial(): Unit = {
    model.allOf[ModelElement].foreach { e =>
      e.colour = calcLocalLevel(e)
    }
  }

  override def runRule(): Unit = {
    model.allOf[ModelElement].foreach { e =>
      e.colour = calcGlobalLevel(e)
    }
  }

  private def calcLocalLevel(element: ModelElement): PrivatiserLevel = {
    val incoming = {
      element.internalIncomingReferences map (_._1)
    }.toSet - element
    println(s"Privatiser for $element START - process ${element.internalIncomingReferences}")

    //is it defined by the signature
    var res: PrivatiserLevel = element match {
      case o: ObjectModel if o.xtends[App] =>
        NoChange(element.symbol, "its an App and needs to be public")
      //      case any: ModelElement if any.hasAnnotation[ExternalAccess] => Some(NoChange(any.getAnnotation[ExternalAccess]))
      //      case method:MethodModel if (method.overidesExternal)=>  res.combine(Level of parent method)
      //probably other cases
      case _ => Undefined(element.symbol)
    }

    incoming foreach { ref =>
      val access = Private(SymbolUtils.findCommonParent(ref.symbol, element.symbol), s"accessed from $ref")
      res = res.combine(access)
    }

    println(s"Privatiser for $element END - $res")
    res
  }
  private def calcGlobalLevel(element: ModelElement): PrivatiserLevel = {
    var res: PrivatiserLevel = element.colour

    //TODO must be at least as visible as anything that we override

    //TODO must be visible to anything that overrides us

    res
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    import scalafix.v1.{Patch => _, _}


    val tv = new SymbolTreeVisitor {

      override protected def handlerSymbol(symbol: Symbol, mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        val prepared = model.fromSymbol[ModelElement](symbol)
        val change = prepared.colour
        change match {
          case NoChange(_, _) => continue
          case Public(_,_) =>
            continue
          case Undefined(_) =>
            (Patch.addLeft(stat, s"/* missed this one!! */"), false)
          case level: PrivatiserLevel =>
            val patch = changeAccessModifier(level, mods, stat, prepared)
            (patch, true)
        }
      }

      def info(sym: Symbol) = doc.info(sym).get.isProtectedWithin


      override protected def handlerPats(pats: Seq[Pat.Var], mods: Seq[Mod], stat: Stat, scope: List[Scope]): (Patch, Boolean) = {
        //for vals and vars we set the access to the broadest of any access of the fields

        val access = pats map (p => (model.fromSymbol[ModelElement](p.symbol)).colour)
        val combined = access.fold[PrivatiserLevel](Undefined(null))((l, r) => l.combine(r))
        combined match {
          case NoChange(_, _) => continue
          case Public(_,_) =>
            continue
          case Undefined(_) =>
            (Patch.addLeft(stat, s"/* missed this one!! */"), false)
          case level: PrivatiserLevel =>
            val patch = changeAccessModifier(level, mods, stat,  model.fromSymbol[ModelElement](pats.head.symbol))
            //we never need to recurse into RHS of decls as they are not externally visible
            (patch, false)
        }
      }

      override def handleImport(importStatement: Import, scope: List[Scope]): (Patch, Boolean) = continue

      private def changeAccessModifier(level: PrivatiserLevel, mods: Seq[Mod], defn: Stat, aModel: ModelElement): Patch =
        (mods.collectFirst {
          case s@Mod.Private(scope) => s
          case s@Mod.Protected(scope) => s
        }, level.asText(aModel)) match {
          case (_, None) => Patch.empty
          case (None, Some(toReplace)) => Patch.addLeft(defn, s"$toReplace ")
          case (Some(existing), Some(toReplace)) => Patch.replaceTree(existing, s"$toReplace ")
        }
    }

    tv.visitDocument(doc.tree)
  }
}
