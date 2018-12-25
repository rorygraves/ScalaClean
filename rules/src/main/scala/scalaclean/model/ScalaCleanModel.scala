package scalaclean.model

import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.{Defn, Pkg, Source, Tree}
import scala.reflect.ClassTag

sealed trait ModelElement {
  def symbol: Symbol

  var colour : Colour = _
  def name: String

  def enclosing: Option[ModelElement]

  val internalOutgoingReferences: List[(ModelElement, Tree)]
  val internalIncomingReferences: List[(ModelElement, Tree)]

    protected def infoTypeName: String
  protected def infoPosString: String = ""
  protected def infoDetail = ""
  protected def infoName = symbol.displayName

  override def toString: String = s"$infoTypeName $infoName [$infoPosString] $infoDetail"
}

sealed trait ClassLike extends ModelElement {
  def fullName: String

  def methods: List[MethodModel]

  def fields: Map[String, FieldModel]
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

        private def processHandler(tree: Tree, handleRes: Boolean): Unit =
          if (handleRes)
            visitChildren(tree)


        def visitTree(tree: Tree): Unit = {

          tree match {
            case pkg: Pkg =>
              visitChildren(tree)
            case obj: Defn.Object =>
              val sym = obj.symbol
              val parent = allKnownObjects.getOrElseUpdate(sym.toString, {
                val res = new ObjectModelImpl(obj, enclosing, doc)
                assert(bySymbol.put(sym, res).isEmpty)
                res
              })
              println(s"object = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Class =>
              val sym = cls.symbol
              val parent = allKnownClasses.getOrElseUpdate(sym.toString, {
                val res = new ClassModelImpl(cls, enclosing, doc)
                assert(bySymbol.put(sym, res).isEmpty)
                res
              })
              println(s"class = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Trait =>
              val sym = cls.symbol
              val parent = allKnownTraits.getOrElseUpdate(sym.toString, {
                val res = new TraitModelImpl(cls, enclosing, doc)
                assert(bySymbol.put(sym, res).isEmpty)
                res
              })
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
              handleOther(other.symbol, other)
              visitChildren(other)
          }
        }

        def handleOther(sym: Symbol, defn: Tree): Boolean = {
          defn match {
            case source: Source => //ignore
            case tree: Tree if tree.symbol.isNone => //ignore
            case tree =>
              enclosing match {
                case Some(parent) =>
                  println(s"*** $parent refers to ${defn.symbol}")
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
      def addRefersTo(tree: Tree): Unit = {
        _refersTo ::= tree
      }
      private var _refersTo = List.empty[Tree]
      private var _refersFrom = List.empty[(ModelElementImpl, Tree)]

      assertBuilding
      elements += this

      private[builder] def build: Unit = {
        _refersTo foreach {
          ref => bySymbol.get(ref.symbol(doc)) foreach {
            _._refersFrom ::= (this, ref)
          }
        }
      }
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

    abstract class FieldModelImpl(defn:Defn, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(defn, enclosing, doc) with FieldModel

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

      def fullName: String = symbol.toString

      private[builder] override def build: Unit = ()
    }

    class ClassModelImpl private[ScalaCleanModel](cls: Defn.Class, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ClassModel {
    }

    class ObjectModelImpl private[ScalaCleanModel](cls: Defn.Object, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ObjectModel {
    }

    class TraitModelImpl private[ScalaCleanModel](cls: Defn.Trait, enclosing: Option[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with TraitModel {
    }

  }

}