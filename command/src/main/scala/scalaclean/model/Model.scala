package scalaclean.model

import scalafix.v1.{Symbol, SymbolInformation}

//import scala.meta.{Decl, Defn, Tree}
import scala.reflect.ClassTag

sealed trait ModelElement extends Ordered[ModelElement]{

  override def compare(that: ModelElement): Int = symbol.value.compare(that.symbol.value)

  def symbol: Symbol

  var mark : Mark = _
  def name: String

  //usually just one element. Can be >1 for  RHS of a val (a,b,c) = ...
  //where a,b,c are the enclosing
  def enclosing: List[ModelElement]
  def classOrEnclosing: ClassLike

  //start target APIs
  def outgoingReferences: Iterable[Refers] = allOutgoingReferences map (_._2)
  def overrides: Iterable[Overrides] = {
    val direct = (allDirectOverrides map (_._2)).toSet
    val thisSym = symbol
    allTransitiveOverrides map {
      case (_, sym) => new impl.OverridesImpl(thisSym, sym, direct.contains(sym))
    }
  }
  //should be in the following form
//  def outgoingReferences: Iterable[Refers]
//  def outgoingReferencedInternal[T <: ModelElement]: Iterable[T]
//  def outgoingReferencedExternal: Iterable[Symbol]
//
//  def incomingReferences: Iterable[Refers]
//  def incomingReferencedInternal[T <: ModelElement]: Iterable[T]

  //end target APIs

  //start old APIs
  def internalOutgoingReferences: List[(ModelElement, Refers)]
  def internalIncomingReferences: List[(ModelElement, Refers)]
  def allOutgoingReferences: List[(Option[ModelElement], Refers)]

  def internalDirectOverrides: List[ModelElement]
  def internalTransitiveOverrides: List[ModelElement]

  def allDirectOverrides: List[(Option[ModelElement], Symbol)]
  def allTransitiveOverrides: List[(Option[ModelElement], Symbol)]

  def internalDirectOverriddenBy: List[ModelElement]
  def internalTransitiveOverriddenBy: List[ModelElement]
  //end old APIs

  def symbolInfo: SymbolInformation
  def symbolInfo(anotherSymbol: Symbol): SymbolInformation

  //any block may contain many val of the same name!
  //  val foo = {
  //    if (1 == 1) {
  //      val x = 2
  //      x
  //    } else {
  //      val x = 3
  //      x
  //    }
  //  }
  def fields: List[FieldModel]
  def methods:  List[MethodModel]
  def innerClassLike: Seq[ClassLike]


  protected def infoTypeName: String
  protected def infoPosString: String
  protected def infoDetail = ""
  protected def infoName = symbol.displayName

  override def toString: String = s"$infoTypeName $infoName [$infoPosString] $infoDetail"
}

sealed trait ClassLike extends ModelElement {
  def fullName: String

  def xtends[T](implicit cls: ClassTag[T]): Boolean
  def xtends(symbol: Symbol): Boolean

  def directExtends: Set[Symbol]
  def transitiveExtends: Set[Symbol]

  def directExtendedBy: Set[ClassLike]
  def transitiveExtendedBy: Set[ClassLike]
}

sealed trait ClassModel extends ClassLike {
  override protected final def infoTypeName: String = "ClassModel"
}
sealed trait ObjectModel extends ClassLike with FieldModel{
  override protected final def infoTypeName: String = "ObjectModel"
  final override def otherFieldsInSameDeclaration = Nil
  type fieldType  = ObjectModel
}

sealed trait TraitModel extends ClassLike{
  override protected final def infoTypeName: String = "TraitModel"
}

sealed trait MethodModel extends ModelElement {
  override protected final def infoTypeName: String = "MethodModel"
}

sealed trait FieldModel extends ModelElement{
  type fieldType <: FieldModel
  def otherFieldsInSameDeclaration: Seq[fieldType]
}

sealed trait ValModel extends FieldModel{
  def isLazy: Boolean
  type fieldType = ValModel
  override protected final def infoTypeName: String = "ValModel"
}

sealed trait VarModel extends FieldModel{
  type fieldType = VarModel
  override protected final def infoTypeName: String = "VarModel"
}
trait ProjectModel {

  def fromSymbol[T <: ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): T

  def fromSymbolLocal[T <: ModelElement](symbol: Symbol, startPos: Int, endPos: Int)(implicit tpe: ClassTag[T]): T = {
    fromSymbol(symbol)
  }


  def getSymbol[T <: ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): Option[T]

  def size: Int
  def allOf[T <: ModelElement : ClassTag]: Iterator[T]
  def printStructure() = allOf[ClassLike] foreach {
    cls => println(s"class ${cls.fullName}")
  }
}


package impl {

  case class BasicElementInfo(symbol: Symbol, source: SourceData, startPos: Int, endPos: Int)
  case class BasicRelationshipInfo(
                                    refers: Map[Symbol, List[RefersImpl]],
                                    extnds: Map[Symbol, List[ExtendsImpl]],
                                    overrides: Map[Symbol, List[OverridesImpl]],
                                    within: Map[Symbol, List[WithinImpl]]) {
    def complete(elements: Map[Symbol, ElementModelImpl]): Unit ={
      refers.values.foreach(_.foreach(_.complete(elements)))
      extnds.values.foreach(_.foreach(_.complete(elements)))
      overrides.values.foreach(_.foreach(_.complete(elements)))
      within.values.foreach(_.foreach(_.complete(elements)))
    }

    def byTo = {
      def byToSymbol[T <: Reference](from: Map[Symbol, List[T]]): Map[Symbol, List[T]] = {
        from.values.flatten.groupBy(_.toSymbol).map {
          case (k, v) => k -> (v.toList)
        }
      }

      BasicRelationshipInfo(
        byToSymbol(refers),
        byToSymbol(extnds),
        byToSymbol(overrides),
        byToSymbol(within)
      )

    }

    def + (that: BasicRelationshipInfo): BasicRelationshipInfo = {
      val res = BasicRelationshipInfo(
        this.refers ++ that.refers,
        this.extnds ++ that.extnds,
        this.overrides ++ that.overrides,
        this.within ++ that.within
      )
      //there should be no overlaps
      assert(res.refers.size == this.refers.size + that.refers.size)
      assert(res.extnds.size == this.extnds.size + that.extnds.size)
      assert(res.overrides.size == this.overrides.size + that.overrides.size)
      assert(res.within.size == this.within.size + that.within.size)

      res
    }
  }
  trait LegacyReferences {
    self: ElementModelImpl =>

    override def internalOutgoingReferences: List[(ModelElement, Refers)] = {
      refersTo.collect {
        case r if r.toElement.isDefined => (r.toElement.get, r)
      }
    }

    override def internalIncomingReferences: List[(ModelElement, Refers)] = {
      refersFrom.collect {
        case r => (r.fromElement, r)
      }
    }

    override def allOutgoingReferences: List[(Option[ModelElement], Refers)] = {
      refersTo map {
        r => (r.toElement, r)
      }
    }
  }

  trait LegacyOverrides {
    self: ElementModelImpl =>

    override def internalDirectOverrides: List[ModelElement] = {
      overrides collect {
        case o if o.isDirect && o.toElement.isDefined => o.toElement.get
      }
    }

    override def internalTransitiveOverrides: List[ModelElement] = {
      overrides collect {
        case o if o.toElement.isDefined => o.toElement.get
      }
    }

    override def allDirectOverrides: List[(Option[ModelElement], Symbol)] = {
      overrides collect {
        case o if o.isDirect => (o.toElement, o.toSymbol)
      }
    }

    override def allTransitiveOverrides: List[(Option[ModelElement], Symbol)] = {
      overrides collect {
        case o => (o.toElement, o.toSymbol)
      }
    }

    override def internalDirectOverriddenBy: List[ModelElement] = {
      overriden collect {
        case o if o.isDirect => o.fromElement
      }
    }

    override def internalTransitiveOverriddenBy: List[ModelElement] = {
      overriden map {
        _.fromElement
      }
    }
  }

  trait LegacyExtends {
    self: ClassLikeModelImpl =>

    override def directExtends: Set[Symbol] = {
      extnds collect {
        case s if s.isDirect => s.toSymbol
      } toSet
    }

    override def transitiveExtends: Set[Symbol] = {
      extnds map (_.toSymbol) toSet
    }

    override def directExtendedBy: Set[ClassLike] = {
      (extendedBy collect {
        case e if e.isDirect => e.fromElement
      }) toSet
    }

    override def transitiveExtendedBy: Set[ClassLike] = {
      (extendedBy map {
        _.fromElement
      }) toSet
    }

  }

  abstract sealed class ElementModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ModelElement
    with LegacyReferences with LegacyOverrides {
    def complete(elements: Map[Symbol, ElementModelImpl],
                 relsFrom: BasicRelationshipInfo,
                 relsTo: BasicRelationshipInfo): Unit = {
      within = relsFrom.within.getOrElse(symbol, Nil) map {
        _.toElement.get.asInstanceOf[ElementModelImpl]
      }
      children = relsTo.within.getOrElse(symbol, Nil) map {
        _.fromElement.asInstanceOf[ElementModelImpl]
      }
      refersTo = relsFrom.refers.getOrElse(symbol, Nil)
      refersFrom = relsTo.refers.getOrElse(symbol, Nil)
      _overrides = relsFrom.overrides.getOrElse(symbol, Nil)
      overriden = relsTo.overrides.getOrElse(symbol, Nil)
    }

    val source = info.source

    def project = source.project

    def projects = project.projects

    override val symbol = info.symbol

    override def name = symbol.displayName

    //start set by `complete`
    var within: List[ElementModelImpl] = _
    var children: List[ElementModelImpl] = _

    var refersTo: List[Refers] = _
    var refersFrom: List[Refers] = _

    var _overrides: List[Overrides] = _

    override def overrides = _overrides

    var overriden: List[Overrides] = _
    //end set by `complete`

    override def enclosing: List[ElementModelImpl] = within

    override def classOrEnclosing: ClassLike = enclosing.head.classOrEnclosing

    override def symbolInfo: SymbolInformation = project.symbolInfo(this, symbol)

    override def symbolInfo(anotherSymbol: Symbol): SymbolInformation = project.symbolInfo(this, anotherSymbol)

    override def fields: List[FieldModel] = children collect {
      case f: FieldModelImpl => f
    }

    override def methods: List[MethodModel] = children collect {
      case m: MethodModelImpl => m
    }

    override def innerClassLike: Seq[ClassLike] = children collect {
      case c: ClassLikeModelImpl => c
    }

    protected def typeName: String

    //  def treeType: ClassTag[ _ <: Tree]
    //  def tree = {
    //    val tag = treeType
    //    source.treeAt(offsetStart, offsetEnd)(tag)
    //  }
    private val offsetStart = info.startPos
    private val offsetEnd = info.endPos

    override protected def infoPosString: String = {
      s"${offsetStart}-${offsetEnd}"

      // TODO Disabled as we don't want to laod the tree to work out line / column right now
      //    val pos = tree.pos
      //    s"${pos.startLine}:${pos.startColumn} - ${pos.endLine}:${pos.endColumn}"
      //
    }
  }

  abstract sealed class ClassLikeModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo) extends ElementModelImpl(info, relationships)
    with ClassLike with LegacyExtends {

    var extnds: List[Extends] = _
    var extendedBy: List[Extends] = _

    override def complete(elements: Map[Symbol, ElementModelImpl],
                          relsFrom: BasicRelationshipInfo,
                          relsTo: BasicRelationshipInfo): Unit = {
      super.complete(elements, relsFrom, relsTo)
      extnds = relsFrom.extnds.getOrElse(symbol, Nil)
      extendedBy = relsTo.extnds.getOrElse(symbol, Nil)

    }

    override def classOrEnclosing: ClassLike = this

    override def fullName: String = ???

    override def xtends[T](implicit cls: ClassTag[T]): Boolean = {
      //TODO what is the canonocal conversion?
      xtends(Symbol(cls.runtimeClass.getName.replace('.', '/') + "#"))
    }

    override def xtends(symbol: Symbol): Boolean = {
      extnds.exists(_.toSymbol == symbol)
    }

  }

  abstract sealed class FieldModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                                       val fieldName: String, val isAbstract: Boolean)
    extends ElementModelImpl(info, relationships) with FieldModel {
    override def otherFieldsInSameDeclaration: Seq[fieldType] = ???
  }

  object ClassModelImpl {
    //  val treeType = ClassTag[Defn.Class](classOf[Defn.Class])
  }

  class ClassModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
    extends ClassLikeModelImpl(info, relationships) with ClassModel {
    override protected def typeName: String = "class"

    //  override def treeType: ClassTag[Defn.Class] = ClassModelImpl.treeType
  }

  object ObjectModelImpl {
    //  val treeType = ClassTag[Defn.Object](classOf[Defn.Object])
  }

  class ObjectModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
    extends ClassLikeModelImpl(info, relationships) with ObjectModel {
    override protected def typeName: String = "object"

    //  override def treeType: ClassTag[Defn.Object] = ObjectModelImpl.treeType
  }

  object TraitModelImpl {
    //  val treeType = ClassTag[Defn.Trait](classOf[Defn.Trait])
  }

  class TraitModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo)
    extends ClassLikeModelImpl(info, relationships) with TraitModel {
    override protected def typeName: String = "trait"

    //  override def treeType: ClassTag[Defn.Trait] = TraitModelImpl.treeType
  }

  object MethodModelImpl {
    //  val treeAbstractType = ClassTag[Decl.Def](classOf[Decl.Def])
    //  val treeConcreteType = ClassTag[Defn.Def](classOf[Defn.Def])
  }

  class MethodModelImpl(info: BasicElementInfo, relationships: BasicRelationshipInfo,
                        val methodName: String, val isAbstract: Boolean, val hasDeclaredType: Boolean)
    extends ElementModelImpl(info, relationships) with MethodModel {
    override protected def typeName: String = "def"

    //  override def treeType: ClassTag[_ <: Tree] =
    //    if (isAbstract) MethodModelImpl.treeAbstractType
    //    else MethodModelImpl.treeConcreteType
  }

  object ValModelImpl {
    //  val treeAbstractType = ClassTag[Decl.Val](classOf[Decl.Val])
    //  val treeConcreteType = ClassTag[Defn.Val](classOf[Defn.Val])
  }

  class ValModelImpl(val info: BasicElementInfo, relationships: BasicRelationshipInfo,
                     fieldName: String, isAbstract: Boolean, val isLazy: Boolean)
    extends FieldModelImpl(info, relationships, fieldName, isAbstract) with ValModel {
    override protected def typeName: String = "trait"

    protected override def infoDetail = s"${super.infoDetail}lazy=$isLazy"

    //  override def treeType: ClassTag[_ <: Tree] =
    //    if (isAbstract) ValModelImpl.treeAbstractType
    //    else ValModelImpl.treeConcreteType
  }

  object VarModelImpl {
    //  val treeAbstractType = ClassTag[Decl.Var](classOf[Decl.Var])
    //  val treeConcreteType = ClassTag[Defn.Var](classOf[Defn.Var])
  }

  class VarModelImpl(val info: BasicElementInfo, relationships: BasicRelationshipInfo,
                     fieldName: String, isAbstract: Boolean)
    extends FieldModelImpl(info, relationships, fieldName, isAbstract) with VarModel {
    override protected def typeName: String = "var"

    //  override def treeType: ClassTag[_ <: Tree] =
    //    if (isAbstract) VarModelImpl.treeAbstractType
    //    else VarModelImpl.treeConcreteType
  }

}