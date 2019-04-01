package scalaclean.model.impl.v2

import scalaclean.model.Utils
import scalafix.v1._

import scala.meta.{Decl, Defn, Pkg, Source, Term, Tree}

class ParserTreeWalker(parser: ParserImpl, implicit val doc: SemanticDocument) {

  def analyse = visitTree(doc.tree)

  private def debug(s: => String) = if (false) println(s)

  private var enclosing = List.empty[ParsedElement]

  private def recordAndVisitChildren(element: ParsedElement, tree: Tree): Unit = {
    recordAndVisitChildren(List(element), tree)
  }

  private def recordAndVisitChildren(elements: List[ParsedElement], tree: Tree): Unit = {
    elements foreach {
      element =>
        parser.additionalDataBuilder.buildAdditionalData(element)
        parser.record(element)
    }
    val prev = enclosing
    enclosing = elements
    visitChildren(tree)
    enclosing = prev
  }

  private def visitChildren(tree: Tree): Unit = {
    tree.children.foreach {
      child =>
        visitTree(child)
    }
  }

  private def visitTree(tree: Tree): Unit = {
    val sym = tree.symbol(doc)
    if (!sym.isNone)
      debug(s"${tree.pos.startLine}:${tree.pos.startColumn} - ${tree.pos.endLine}:${tree.pos.endColumn} ${tree.symbol(doc)}")

    tree match {
      case _: Pkg =>
        visitChildren(tree)
      case _: Source =>
        visitChildren(tree)
      case obj: Defn.Object =>
        val sym = obj.symbol
        val parent = new ParsedObjectImpl(obj, enclosing, doc)
        recordAndVisitChildren(parent, tree)
      case cls: Defn.Class =>
        val sym = cls.symbol
        val parent = new ParsedClassImpl(cls, enclosing, doc)
        recordAndVisitChildren(parent, tree)
      case cls: Defn.Trait =>
        val sym = cls.symbol
        val parent = new ParsedTraitImpl(cls, enclosing, doc)
        recordAndVisitChildren(parent, tree)
      case method: Defn.Def =>
        val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
        val fullSig = s"${method.symbol}:$typeSigs"
        val parent = new ParsedMethodDefn(method, enclosing, doc)
        recordAndVisitChildren(parent, tree)
      case method: Decl.Def =>
        val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
        val fullSig = s"${method.symbol}:$typeSigs"
        val parent = new ParsedMethodDecl(method, enclosing, doc)
        recordAndVisitChildren(parent, tree)
      case valDef: Defn.Val =>
        //to cope with  val (x,Some(y)) = ....
        val fields = Utils.readVars(valDef.pats)
        val fieldModels = fields map {
          new ParsedValDefn(valDef, _, fields, enclosing, doc)
        }
        recordAndVisitChildren(fieldModels, tree)
      case valDef: Decl.Val =>
        //to cope with  val (x,Some(y)) = ....
        val fields = Utils.readVars(valDef.pats)
        val fieldModels = fields map {
          new ParsedValDecl(valDef, _, fields, enclosing, doc)
        }
        recordAndVisitChildren(fieldModels, tree)
      case varDef: Defn.Var =>
        //to cope with  var (x,Some(y)) = ....
        val fields = Utils.readVars(varDef.pats)
        val fieldModels = fields map {
          new ParsedVarDefn(varDef, _, fields, enclosing, doc)
        }
        recordAndVisitChildren(fieldModels, tree)
      case varDef: Decl.Var =>
        //to cope with  var (x,Some(y)) = ....
        val fields = Utils.readVars(varDef.pats)
        val fieldModels = fields map {
          new ParsedVarDecl(varDef, _, fields, enclosing, doc)
        }
        recordAndVisitChildren(fieldModels, tree)
      case _ =>
        val thisTreeSymbol = tree.symbol
        foundSymbol(tree.symbol, tree)
        visitChildren(tree)
    }
    //and synthetics
    tree match {
      case term: Term =>
        term.synthetic foreach (visitSynthetic(_, term))
      case _ => //ignore
    }
  }

  private def foundSymbol(symbol: Symbol, tree: Tree): Unit = {
    if (!symbol.isNone)
      if (enclosing.isEmpty) {
        debug(s"cant add to parent  ${tree.getClass} ${tree.pos.start} .. ${tree.pos.end} synthetic:$inSynthetic - ${symbol}")
      } else {
        if (enclosing.forall(_.symbol != symbol))
          enclosing foreach {
            _.addRefersTo(tree, symbol, inSynthetic)
          }
      }
  }

  private var inSynthetic = false

  private def visitSynthetic(tree: SemanticTree, orig: Tree): Unit = {
    val wasInSynthetic = inSynthetic
    inSynthetic = true
    tree match {
      case NoTree =>
      case LiteralTree(constant: Constant) =>

      case IdTree(info: SymbolInformation) =>
        foundSymbol(info.symbol, orig)

      case SelectTree(qualifier: SemanticTree, id: IdTree) =>
        visitSynthetic(qualifier, orig)
        visitSynthetic(id, orig)

      case ApplyTree(function: SemanticTree, arguments: List[SemanticTree]) =>
        visitSynthetic(function, orig)
        arguments foreach (visitSynthetic(_, orig))

      case TypeApplyTree(function: SemanticTree, typeArguments: List[SemanticType]) =>
        visitSynthetic(function, orig)
        typeArguments foreach (visitType(_, orig))

      case FunctionTree(parameters: List[IdTree], body: SemanticTree) =>
        parameters foreach (visitSynthetic(_, orig))
        visitSynthetic(body, orig)

      case MacroExpansionTree(beforeExpansion: SemanticTree, tpe: SemanticType) =>
        visitSynthetic(beforeExpansion, orig)
        visitType(tpe, orig)

      case OriginalSubTree(tree: scala.meta.Tree) =>
      // we don't visit the original tree - that how we got here
      //  so we have traversed it already
      // or will when we return the the tree traversal
      // visitTree(tree)
      case OriginalTree(tree: scala.meta.Tree) =>
      // we don't visit the original tree - that how we got here
      //  so we have traversed it already
      // or will when we return the the tree traversal
      // visitTree(tree)
    }
    inSynthetic = wasInSynthetic
  }

  private def visitType(tpe: SemanticType, tree: Tree): Unit = tpe match {

    case TypeRef(prefix: SemanticType, symbol: Symbol, typeArguments: List[SemanticType]) =>
      visitType(prefix, tree)
      foundSymbol(symbol, tree)
      typeArguments foreach {
        visitType(_, tree)
      }
    case SingleType(prefix: SemanticType, symbol: Symbol) =>
      visitType(prefix, tree)
      foundSymbol(symbol, tree)
    case ThisType(symbol: Symbol) =>
      foundSymbol(symbol, tree)
    case SuperType(prefix: SemanticType, symbol: Symbol) =>
      visitType(prefix, tree)
      foundSymbol(symbol, tree)
    case ConstantType(constant: Constant) =>
    case IntersectionType(types: List[SemanticType]) =>
      types foreach {
        visitType(_, tree)
      }
    case UnionType(types: List[SemanticType]) =>
      types foreach {
        visitType(_, tree)
      }
    case WithType(types: List[SemanticType]) =>
      types foreach {
        visitType(_, tree)
      }
    case StructuralType(tpe: SemanticType, declarations: List[SymbolInformation]) =>
      visitType(tpe, tree)
      //not sure it it worth visiting these
      declarations foreach {
        i =>
          foundSymbol(i.symbol, tree)
      }
    case AnnotatedType(annotations: List[Annotation], tpe: SemanticType) =>
      annotations foreach { a => visitType(a.tpe, tree) }
      visitType(tpe, tree)
    case ExistentialType(tpe: SemanticType, declarations: List[SymbolInformation]) =>
      visitType(tpe, tree)
      declarations foreach {
        i =>
          foundSymbol(i.symbol, tree)
      }
    case UniversalType(typeParameters: List[SymbolInformation], tpe: SemanticType) =>
      typeParameters foreach {
        i =>
          foundSymbol(i.symbol, tree)
      }
      visitType(tpe, tree)
    case ByNameType(tpe: SemanticType) =>
      visitType(tpe, tree)
    case RepeatedType(tpe: SemanticType) =>
      visitType(tpe, tree)
    case NoType =>
  }
}
