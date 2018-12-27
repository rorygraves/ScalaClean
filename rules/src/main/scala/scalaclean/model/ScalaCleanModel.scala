package scalaclean.model

import java.net.URLClassLoader

import scalafix.internal.v1.InternalSemanticDoc
import scalafix.v1.{SemanticDocument, Symbol, SymbolInformation}

import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.{Defn, Pkg, Source, Template, Tree}
import scala.reflect.ClassTag
import scala.reflect.api.Mirror
import scala.reflect.runtime.JavaUniverse


sealed trait ModelElement {
  def symbol: Symbol

  var colour : Colour = _
  def name: String

  def enclosing: Option[ModelElement]

  val internalOutgoingReferences: List[(ModelElement, Tree)]
  val internalIncomingReferences: List[(ModelElement, Tree)]

  def directOverrides: List[Symbol]
  def transitiveOverrides: List[Symbol]

  def directOverriddenBy: List[ModelElement]
  def transitiveOverriddenBy: List[ModelElement]

  def symbolInfo: SymbolInformation

  protected def infoTypeName: String
  protected def infoPosString: String
  protected def infoDetail = ""
  protected def infoName = symbol.displayName

  override def toString: String = s"$infoTypeName $infoName [$infoPosString] $infoDetail"
}

sealed trait ClassLike extends ModelElement {
  def fullName: String

  def methods: List[MethodModel]

  def fields: Map[String, FieldModel]
  def xtends[T](implicit cls: ClassTag[T]): Boolean
  def xtends(symbol: Symbol): Boolean

  def directExtends: Set[Symbol]
  def transitiveExtends: Set[Symbol]

  def directExtendedBy: Set[ClassLike]
  def transitiveExtendedBy: Set[ClassLike]
}

sealed trait ClassModel extends ClassLike {
  override protected def infoTypeName: String = "class"
}
sealed trait ObjectModel extends ClassLike{

  override protected def infoTypeName: String = "object"
}

sealed trait TraitModel extends ClassLike{
  override protected def infoTypeName: String = "trait"
}

sealed trait MethodModel extends ModelElement {
  override protected def infoTypeName: String = "def"
}

sealed trait FieldModel extends ModelElement

sealed trait ValModel extends FieldModel{
  override protected def infoTypeName: String = "val"
}

sealed trait VarModel extends FieldModel{
  override protected def infoTypeName: String = "var"
}

class ScalaCleanModel {
  def fromSymbol[T <: ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): T =
    builder.bySymbol.get(symbol) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x) if tpe.runtimeClass.isInstance(x) => x.asInstanceOf[T]
      case Some(x) => throw new IllegalArgumentException(s"Unexxpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
    }

  def printStructure() = allOf[ClassLike] foreach {
    cls => println(s"class ${cls.fullName}")
  }

  def analyse(implicit doc: SemanticDocument) = builder.analyse

  import builder._

  def size = ModelBuilder.elements.result().size

  def finishedParsing(): Unit = {
    ModelBuilder.finishedParsing()
  }

  private lazy val all = {
    ModelBuilder.assertBuildModelFinished()
    ModelBuilder.elements.result()
  }

  def allOf[T <: ModelElement](implicit cls: ClassTag[T]): List[T] = {
    all collect {
      case wanted if cls.runtimeClass.isInstance(wanted) => wanted.asInstanceOf[T]
    }
  }

  private object builder {

    import ModelBuilder._
    import scalafix.v1._

    import collection.mutable


    val allKnownClasses = mutable.Map[String, ClassModelImpl]()
    val allKnownObjects = mutable.Map[String, ObjectModelImpl]()
    val allKnownTraits = mutable.Map[String, TraitModelImpl]()
    val bySymbol = mutable.Map[Symbol, ModelElementImpl]()

    private var enclosing = Option.empty[ModelElementImpl]
    private object access {
      val internalAccess = classOf[SemanticDocument].getMethod("internal")
      val classpathAccess = classOf[GlobalSymbolTable].getField("classpath")
    }
    def internalDoc(doc: SemanticDocument) =
      access.internalAccess.invoke(doc).asInstanceOf[InternalSemanticDoc]
    def symbolTable(doc: SemanticDocument) =
      internalDoc(doc).symtab
    private [this] val cachedClassLoader = new java.util.IdentityHashMap[GlobalSymbolTable, JavaUniverse#JavaMirror]

    def mirror(doc: SemanticDocument): JavaUniverse#JavaMirror = {
      val symbolTable = internalDoc(doc).symtab.asInstanceOf[GlobalSymbolTable]
      cachedClassLoader.computeIfAbsent(symbolTable, {
        symbolTable =>

          import scala.meta.io._
          val classpath = access.classpathAccess.get(symbolTable).asInstanceOf[Classpath]
          val urls = classpath.shallow.map {
            path => path.toURI.toURL
          }
          val cl = new URLClassLoader(urls.toArray)
          val ru = scala.reflect.runtime.universe
          ru.runtimeMirror(cl).asInstanceOf[JavaUniverse#JavaMirror]
      })
    }

    def analyse(implicit doc: SemanticDocument) = {
      assertBuilding()
      object analysisVisitor {

        final def visitDocument(tree: Tree): Unit = {
          visitTree(tree)
        }

        private def visitEnclosingChildren(parent: ModelElementImpl, t: Tree): Unit = {
          val prev = enclosing
          enclosing = Some(parent)
          visitChildren(t)
          enclosing = prev
        }

        private def visitChildren(t: Tree): Unit = {
          t.children.foreach {
            visitTree
          }
        }

        def visitTree(tree: Tree): Unit = {

          tree match {
            case pkg: Pkg =>
              visitChildren(tree)
            case obj: Defn.Object =>
              val sym = obj.symbol
              val parent = allKnownObjects.getOrElseUpdate(sym.toString, new ObjectModelImpl(obj, enclosing, doc))
              println(s"object = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Class =>
              val sym = cls.symbol
              val parent = allKnownClasses.getOrElseUpdate(sym.toString, new ClassModelImpl(cls, enclosing, doc))
              println(s"class = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Trait =>
              val sym = cls.symbol
              val parent = allKnownTraits.getOrElseUpdate(sym.toString, new TraitModelImpl(cls, enclosing, doc))
              println(s"trait = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case method: Defn.Def =>
              val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
              val fullSig = s"${method.symbol}:$typeSigs"
              val parent = new MethodModelImpl(method, enclosing, doc)
              visitEnclosingChildren(parent, tree)
            case valDef: Defn.Val =>
              val parent = new ValModelImpl(valDef, enclosing, doc)
              visitEnclosingChildren(parent, tree)
            case varDef: Defn.Var =>
              val parent = new VarModelImpl(varDef, enclosing, doc)
              visitEnclosingChildren(parent, tree)
            case other: Tree =>
              handleOther(other)
              visitChildren(other)
          }
        }

        def handleOther(defn: Tree): Boolean = {
          defn match {
            case source: Source => //ignore
            case tree: Tree if tree.symbol.isNone => //ignore
            case tree =>
              enclosing match {
                case Some(parent) if parent.symbol == defn.symbol => //ignore internal refs
                case Some(parent) =>
                  parent.addRefersTo(defn)
                case None =>
                  val pos = defn.pos
                  println(s"XXX cant add to parent = ${defn.getClass} ${pos.start} .. ${pos.end} - ${defn.symbol}")
              }
          }
          true
        }
      }
      analysisVisitor.visitDocument(doc.tree)
    }

    object ModelBuilder {

      def assertBuildModelFinished() = assert(!_building)

      def assertBuilding() = assert(_building)

      private var _building = true

      def finishedParsing(): Unit = {
        assertBuilding
        _building = false
        elements.result() foreach (_.build)
      }

      private[ScalaCleanModel] val elements = List.newBuilder[ModelElementImpl]

    }


    sealed abstract class ModelElementImpl(protected val defn: Defn, val enclosing: Option[ModelElementImpl], protected val doc: SemanticDocument) extends ModelElement {
      assertBuilding()
      assert(bySymbol.put(symbol, this).isEmpty)

      def addRefersTo(tree: Tree): Unit = {
        _refersTo ::= tree
      }

      override protected def infoPosString: String = {
        val pos = defn.pos
        s"${pos.startLine}:${pos.startColumn} - ${pos.endLine}:${pos.endColumn}"
      }

      private var _refersTo = List.empty[Tree]
      private var _refersFrom = List.empty[(ModelElementImpl, Tree)]

      assertBuilding
      elements += this

      private[builder] def build: Unit = {
        _refersTo foreach {
          ref =>
            val referred = ref.symbol(doc)
            val symToRef = bySymbol.get(referred)
            symToRef foreach {
              _._refersFrom ::= (this, ref)
            }
        }
      }


      override def symbolInfo: SymbolInformation = doc.info(symbol).get

      override lazy val internalOutgoingReferences: List[(ModelElementImpl, Tree)] = {
        assertBuildModelFinished
        for (tree <- _refersTo;
             ref <- bySymbol.get(tree.symbol(doc))) yield {
          (ref, tree)
        }
      }
      override lazy val internalIncomingReferences: List[(ModelElementImpl, Tree)] = {
        assertBuildModelFinished
        _refersFrom
      }
      private var _directOverrides = List.empty[Symbol]
      private var _directOverrided = List.empty[ModelElementImpl]
      override def directOverrides: List[Symbol] = {
        assertBuildModelFinished()
        _directOverrides
      }
      override def transitiveOverrides: List[Symbol] = {
        assertBuildModelFinished()
        ???
      }
      override def directOverriddenBy: List[ModelElement] = {
        assertBuildModelFinished()
        _directOverrided
      }
      override lazy val transitiveOverriddenBy: List[ModelElement] = {
        directOverriddenBy ::: directOverriddenBy flatMap (_.transitiveOverriddenBy)
      }
      protected def recordOverrides(s: Symbol) = {
        assertBuilding()
        _directOverrides ::= s
      }
      //record overrides
      //for a method to override it must have a parent which is a class/trait
//      enclosing match {
//        case Some(cls: ClassLikeImpl) =>
//          val jm = mirror(doc)
//          val cls = jm.classToScala(jm.classLoader.loadClass(cls.asJavaClassName))
//          jm.typeOf(TypeTag(jm.classToScala(cls)))
//          jm.typeToScala(jm.classToScala(cls).)
//
//      }

      //TODO detect override and update _directOverrides && _directOverrided

      enclosing foreach {
        _._children ::= this
      }
      private var _children = List.empty[ModelElementImpl]

      private[ScalaCleanModel] def children = _children

      private[ScalaCleanModel] def enclosingClassLike: Option[ClassLike] = {
        enclosing match {
          case None => None
          case Some(classLike: ClassLike) => Some(classLike)
          case Some(other: ModelElementImpl) => other.enclosingClassLike
        }
      }

      override def symbol: Symbol = defn.symbol(doc)

      def name = symbol.displayName

    }

    abstract class FieldModelImpl(defn: Defn, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(defn, enclosing, doc) with FieldModel

    class VarModelImpl(vr: Defn.Var, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends FieldModelImpl(vr, enclosing, doc) with VarModel

    class ValModelImpl(vl: Defn.Val, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends FieldModelImpl(vl, enclosing, doc) with ValModel

    class MethodModelImpl(df: Defn.Def, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(df, enclosing, doc) with MethodModel {
      private[builder] override def build: Unit = super.build
    }

    abstract sealed class ClassLikeImpl(defn: Defn, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(defn, enclosing, doc) with ClassLike {
      lazy val fields: Map[String, FieldModel] = {
        assertBuildModelFinished
        (children collect {
          case field: FieldModelImpl => field.symbol.displayName -> field
        }).toMap
      }
      lazy val methods: List[MethodModel] = {
        assertBuildModelFinished
        children collect {
          case m: MethodModel => m
        }
      }
      lazy val innerClassLike: Seq[ClassLike] = {
        assertBuildModelFinished
        children collect {
          case m: ClassModel => m
        }
      }
      private var _directExtendedBy = Set.empty[ClassLike]
      protected def template: Template

      override val directExtends: Set[Symbol] = (template.inits collect {
        case i => i.symbol(doc)
      }) toSet

      override def transitiveExtends: Set[Symbol] = {
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
        assertBuildModelFinished()
        directExtends.foldLeft(directExtends){
          case (result: Set[Symbol], direct) =>
            result ++ xtends(direct)
        }
      }

      override def directExtendedBy: Set[ClassLike] = {
        assertBuildModelFinished()
        _directExtendedBy
      }

      override def transitiveExtendedBy: Set[ClassLike] = {
        directExtendedBy ++ directExtendedBy flatMap (_.transitiveExtendedBy)
      }

      def fullName: String = symbol.toString

      private[builder] override def build: Unit = {
        super.build
        directExtends flatMap bySymbol.get foreach {
          case c: ClassLikeImpl => c._directExtendedBy += this
          case _ => ??? //cant happen
        }
      }

      override def xtends[T](implicit cls: ClassTag[T]): Boolean = {
        //TODO what is the canonocal conversion?
        xtends(Symbol(cls.runtimeClass.getName.replace('.','/')+"#"))
      }
      override def xtends(symbol: Symbol): Boolean = {
        transitiveExtends.contains(symbol)
      }
    }

    class ClassModelImpl private[ScalaCleanModel](cls: Defn.Class, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ClassModel {
      override protected def template: Template = cls.templ
    }

    class ObjectModelImpl private[ScalaCleanModel](cls: Defn.Object, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ObjectModel{
      override protected def template: Template = cls.templ
    }

    class TraitModelImpl private[ScalaCleanModel](cls: Defn.Trait, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with TraitModel{
      override protected def template: Template = cls.templ
    }

  }

}