package scalaclean.model.impl.v2

import scalaclean.model.Refers
import scalafix.v1._

import scala.meta.{Decl, Defn, Mod, Pat, Stat, Template, Term, Tree, Type}

sealed abstract class ParsedElement(val stat: Stat, val enclosing: List[ParsedElement], val doc: SemanticDocument) {
  assert(!symbol.isNone)

  def addRefersTo(tree: Tree, symbol: Symbol, isSynthetic: Boolean): Unit = {
    _refersTo ::= (symbol, isSynthetic)
  }

  def infoPosString: String = {
    val pos = stat.pos
    s"${pos.startLine}:${pos.startColumn} - ${pos.endLine}:${pos.endColumn}"
  }

  private var _refersTo = List.empty[(Symbol, Boolean)]
  def refersTo = _refersTo

  private var _directOverrides = List.empty[Symbol]
  private var _transitiveOverrides = List.empty[Symbol]

  def recordOverrides(directOverides: List[Symbol], transitiveOverrides: List[Symbol]) = {
    assert(_directOverrides eq Nil)
    _directOverrides = directOverides.distinct
    _transitiveOverrides = (transitiveOverrides ++ directOverides).distinct
  }
  def directOverrides = _directOverrides
  def transitiveOverrides = _transitiveOverrides

  def symbol: Symbol = stat.symbol(doc)

  override def toString = s"${getClass.getSimpleName} $stat$extra"
  protected def extra = ""
}

abstract sealed class ParsedField(defn: Stat, val field: Pat.Var, allFields: Seq[Pat.Var],
                                  enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedElement(defn, enclosing, doc) {
  require(allFields.nonEmpty)

  override def symbol: Symbol = field.symbol(doc)

  def isAbstract: Boolean

  override protected def extra: String = s"${super.extra} ${field.name}"
}

abstract class ParsedVar(vr: Stat, field: Pat.Var, allFields: Seq[Pat.Var],
                         enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedField(vr, field, allFields, enclosing, doc) {
}

class ParsedVarDefn(vr: Defn.Var, field: Pat.Var, allFields: Seq[Pat.Var],
                    enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedVar(vr, field, allFields, enclosing, doc) {

  override def isAbstract: Boolean = false
}

class ParsedVarDecl(vr: Decl.Var, field: Pat.Var, allFields: Seq[Pat.Var],
                    enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedVar(vr, field, allFields, enclosing, doc) {

  override def isAbstract: Boolean = true
}

abstract class ParsedVal(vl: Stat, field: Pat.Var, allFields: Seq[Pat.Var],
                         enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedField(vl, field, allFields, enclosing, doc) {

  def isLazy: Boolean = val_mods.exists(_.isInstanceOf[Mod.Lazy])

  def val_mods: List[Mod]
}

class ParsedValDefn(val valDef: Defn.Val, field: Pat.Var, allFields: Seq[Pat.Var],
                    enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedVal(valDef, field, allFields, enclosing, doc) {
  override def val_mods = valDef.mods

  override def isAbstract: Boolean = false
}

class ParsedValDecl(val valDecl: Decl.Val, field: Pat.Var, allFields: Seq[Pat.Var],
                    enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedVal(valDecl, field, allFields, enclosing, doc) {
  override def val_mods = valDecl.mods

  override def isAbstract: Boolean = true
}

abstract class ParsedMethod(stat: Stat, enclosing: List[ParsedElement], doc: SemanticDocument) extends
  ParsedElement(stat, enclosing, doc) {

  def method_mods: List[Mod]

  def method_name: Term.Name

  def method_tparams: List[Type.Param]

  def method_paramss: List[List[Term.Param]]

  def method_decltpe: Option[Type]

  def isAbstract: Boolean

  def paramsType = method_paramss map (_.map {
    param: Term.Param => param.symbol(doc)
  })
  override protected def extra: String = s"${super.extra} ${method_name}"

}

class ParsedMethodDefn(val defn: Defn.Def, enclosing: List[ParsedElement], doc: SemanticDocument) extends ParsedMethod(defn, enclosing, doc) {
  def method_mods = defn.mods

  def method_name = defn.name

  def method_tparams = defn.tparams

  def method_paramss = defn.paramss

  def method_decltpe = defn.decltpe

  def isAbstract = false
}

class ParsedMethodDecl(val decl: Decl.Def, enclosing: List[ParsedElement], doc: SemanticDocument) extends ParsedMethod(decl, enclosing, doc) {
  def method_mods = decl.mods

  def method_name = decl.name

  def method_tparams = decl.tparams

  def method_paramss = decl.paramss

  def method_decltpe = Some(decl.decltpe)

  def isAbstract = true
}

abstract sealed class ParsedClassLike(defn: Defn, enclosing: List[ParsedElement], doc: SemanticDocument) extends ParsedElement(defn, enclosing, doc) {
  protected def template: Template

  val directExtends: Set[Symbol] = (template.inits collect {
    case i => i.symbol(doc)
  }) toSet

  def transitiveExtends: Set[Symbol] = {
    def xtends(classSym: Symbol): Set[Symbol] = {
      doc.info(classSym) match {
        case None => ???
        case Some(info) => info.signature match {
          case cls: ClassSignature =>
            cls.parents.flatMap({
              case ref: TypeRef => xtends(ref.symbol) + ref.symbol
              case _ => ???
            }) toSet
          case _ => ???
        }
      }
    }

    directExtends.foldLeft(directExtends) {
      case (result: Set[Symbol], direct) =>
        result ++ xtends(direct)
    }
  }

  def fullName: String = symbol.toString

}

class ParsedClassImpl(cls: Defn.Class, enclosing: List[ParsedElement], doc: SemanticDocument) extends ParsedClassLike(cls, enclosing, doc) {
  override protected def template: Template = cls.templ
}

class ParsedObjectImpl(val cls: Defn.Object, enclosing: List[ParsedElement], doc: SemanticDocument) extends ParsedClassLike(cls, enclosing, doc) {
  override protected def template: Template = cls.templ
}

class ParsedTraitImpl(cls: Defn.Trait, enclosing: List[ParsedElement], doc: SemanticDocument) extends ParsedClassLike(cls, enclosing, doc) {
  override protected def template: Template = cls.templ
}

