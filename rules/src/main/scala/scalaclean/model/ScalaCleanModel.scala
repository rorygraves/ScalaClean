package scalaclean.model

import scalaclean.util.{BasicTreeVisitor, DefaultTreeVisitor, Scope, TreeVisitor}
import scalafix.v1.SemanticDocument

import scala.meta.{Defn, Pkg, Source, Stat, Term, Tree}
import scala.reflect.ClassTag

sealed trait ModelElement {
  var colours = List.empty[Colour]

  def enclosing: Option[ModelElement]

  protected def typeName: String
}

sealed trait ClassLike extends ModelElement {
  def fullName: String

  def methods: List[MethodModel]

  def fields: Map[String, FieldModel]
}

sealed trait ClassModel extends ClassLike {
  override protected def typeName: String = "class"
}
sealed trait ObjectModel extends ClassLike{
  override protected def typeName: String = "object"
}

sealed trait TraitModel extends ClassLike{
  override protected def typeName: String = "trait"
}

sealed trait MethodModel extends ModelElement {
  override protected def typeName: String = "def"
  val name: String
}

sealed trait FieldModel extends ModelElement

sealed trait ValModel extends FieldModel{
  override protected def typeName: String = "val"
}

sealed trait VarModel extends FieldModel{
  override protected def typeName: String = "var"
}

class ScalaCleanModel {
  def printStructure() = allOf[ClassLike] foreach {
    cls => println(s"class ${cls.fullName}")
  }

  def analyse(implicit doc: SemanticDocument) = builder.analyse

  import builder._

  def finishedParsing(): Unit = {
    ModelBuilder.finishedParsing()
  }

  private lazy val all = {
    ModelBuilder.assertBuildModelFinished()
    ModelBuilder.elements.result()
  }

  def allOf[T <: ModelElement](implicit cls: ClassTag[T]): List[T] = {
    all collect {
      case wanted: T => wanted
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
                val res = new ObjectModelImpl(sym, obj, enclosing)
                assert(bySymbol.put(sym, res).isEmpty)
                res
              })
              println(s"object = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Class =>
              val sym = cls.symbol
              val parent = allKnownClasses.getOrElseUpdate(sym.toString, {
                val res = new ClassModelImpl(sym, cls, enclosing)
                assert(bySymbol.put(sym, res).isEmpty)
                res
              })
              println(s"class = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Trait =>
              val sym = cls.symbol
              val parent = allKnownTraits.getOrElseUpdate(sym.toString, {
                val res = new TraitModelImpl(sym, cls, enclosing)
                assert(bySymbol.put(sym, res).isEmpty)
                res
              })
              println(s"trait = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case method: Defn.Def =>
              val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
              val fullSig = s"${method.symbol}:$typeSigs"
              val parent = new MethodModelImpl(method.symbol.toString, method, enclosing)
              visitEnclosingChildren(parent, tree)
            case valDef: Defn.Val =>
              val parent = new ValModelImpl(valDef.symbol.toString, valDef, enclosing)
              visitEnclosingChildren(parent, tree)
            case varDef: Defn.Var =>
              val parent = new VarModelImpl(varDef.symbol.toString, varDef, enclosing)
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
                  println(s"*** other add to $parent = $defn ${defn.symbol}")
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


    sealed abstract class ModelElementImpl(val enclosing: Option[ModelElementImpl]) extends ModelElement {
      assertBuilding
      elements += this

      private[builder] def build: Unit = ()

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

    }

    abstract class FieldModelImpl(val name: String, enclosing: Option[ModelElementImpl]) extends ModelElementImpl(enclosing) with FieldModel

    class VarModelImpl(name: String, vr: Defn.Var, enclosing: Option[ModelElementImpl]) extends FieldModelImpl(name, enclosing) with VarModel

    class ValModelImpl(name: String, vl: Defn.Val, enclosing: Option[ModelElementImpl]) extends FieldModelImpl(name, enclosing) with ValModel

    class MethodModelImpl(val name: String, df: Defn.Def, enclosing: Option[ModelElementImpl]) extends ModelElementImpl(enclosing) with MethodModel {


      private[builder] override def build: Unit = super.build
    }

    abstract sealed class ClassLikeImpl(val sym: Symbol, enclosing: Option[ModelElementImpl]) extends ModelElementImpl(enclosing) with ClassLike {
      lazy val fields: Map[String, FieldModel] = {
        assertBuildModelFinished
        (children collect {
          case field: FieldModelImpl => field.name -> field
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

      def fullName: String = sym.toString

      private[builder] override def build: Unit = ()
    }

    class ClassModelImpl private[ScalaCleanModel](sym: Symbol, cls: Defn.Class, encl: Option[ModelElementImpl]) extends ClassLikeImpl(sym, enclosing) with ClassModel {
    }

    class ObjectModelImpl private[ScalaCleanModel](sym: Symbol, cls: Defn.Object, encl: Option[ModelElementImpl]) extends ClassLikeImpl(sym, enclosing) with ObjectModel {
    }

    class TraitModelImpl private[ScalaCleanModel](sym: Symbol, trt: Defn.Trait, encl: Option[ModelElementImpl]) extends ClassLikeImpl(sym, enclosing) with TraitModel {
    }

  }

}