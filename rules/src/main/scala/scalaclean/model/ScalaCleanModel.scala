package scalaclean.model

import scalaclean.util.{BasicTreeVisitor, DefaultTreeVisitor, Scope, TreeVisitor}
import scalafix.v1.SemanticDocument

import scala.meta.{Defn, Pkg, Source, Stat, Term}
import scala.reflect.ClassTag

sealed trait ModelElement{
  var colours = List.empty[Colour]
  def enclosing: Option[ModelElement]
}

sealed trait ClassLike extends ModelElement{
  def fullName: String
  def methods: List[MethodModel]
  def fields: Map[String, FieldModel]
}
sealed trait ClassModel extends ClassLike
sealed trait ObjectModel extends ClassLike
sealed trait TraitModel extends ClassLike

sealed trait MethodModel extends ModelElement {
  val name:String
}
sealed trait CodeModel extends ModelElement

sealed trait FieldModel extends ModelElement
sealed trait ValModel extends FieldModel
sealed trait VarModel extends FieldModel

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
      object analysisVisitor extends BasicTreeVisitor() {

        val continue = true

        override type Child = Option[ModelElementImpl]
        override protected def beforeChildren: Child = enclosing
        override protected def afterChildren(prev: Child): Unit = enclosing = prev

        override def handleVar(symbol: Symbol, varDef: Defn.Var): Boolean = {
          new VarModelImpl(symbol.toString, varDef, enclosing)
          continue
        }

        override def handleVal(symbol: Symbol, valDef: Defn.Val): Boolean = {
          new ValModelImpl(symbol.toString, valDef, enclosing)
          continue
        }

        override def handlePackage(packageName: Term.Name, pkg: Pkg): Boolean = {
          continue
        }

        override def handleMethod(symbol: Symbol, fullSig: String, method: Defn.Def): Boolean = {
          new MethodModelImpl(symbol.toString, method, enclosing)
          continue
        }

        override def handleObject(sym: Symbol, obj: Defn.Object): Boolean = {
          val resolved = allKnownObjects.getOrElseUpdate(sym.toString, {
            val res = new ObjectModelImpl(sym, obj, enclosing)
            assert(bySymbol.put(sym, res).isEmpty)
            res
          })
          println(s"object = ${resolved.fullName}")
          continue
        }

        override def handleTrait(sym: Symbol, obj: Defn.Trait): Boolean = {
          val resolved = allKnownTraits.getOrElseUpdate(sym.toString, {
            val res = new TraitModelImpl(sym, obj, enclosing)
            assert(bySymbol.put(sym, res).isEmpty)
            res
          })
          println(s"object = ${resolved.fullName}")
          continue
        }

        override def handleClass(sym: Symbol, cls: Defn.Class): Boolean = {
          val resolved = allKnownClasses.getOrElseUpdate(sym.toString, {
            val res = new ClassModelImpl(sym, cls, enclosing)
            assert(bySymbol.put(sym, res).isEmpty)
            res
          })
          println(s"class = ${resolved.fullName}")
          continue
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


    sealed abstract class ModelElementImpl(val enclosing: Option[ModelElementImpl]) extends  ModelElement{
      assertBuilding
      elements += this

      private[builder] def build: Unit = ()

      enclosing foreach {
        _._children ::= this
      }
      private var _children = List.empty[ModelElementImpl]
      private[ScalaCleanModel] def children = _children

    }

    abstract class FieldModelImpl(val name: String, enclosing: Option[ModelElementImpl]) extends ModelElementImpl(enclosing) with FieldModel {
      def initialiser = {
        assertBuildModelFinished
        _initialiser
      }

      private var _initialiser = Option.empty[CodeModel]

      private[builder] def initialiser_=(codeModel: CodeModel): Unit = {
        assertBuilding
        assert(_initialiser isEmpty)
        assert(codeModel._owner isEmpty)
        codeModel._owner = this
        _initialiser = Some(codeModel)
      }

      private[builder] override def build: Unit = super.build

    }
    class VarModelImpl(name: String, vr: Defn.Var, enclosing: Option[ModelElementImpl]) extends FieldModelImpl(name, enclosing) with VarModel
    class ValModelImpl(name: String, vl: Defn.Val, enclosing: Option[ModelElementImpl]) extends FieldModelImpl(name, enclosing) with ValModel

    class MethodModelImpl(val name: String, df: Defn.Def, enclosing: Option[ModelElementImpl]) extends ModelElementImpl(enclosing) with MethodModel {

      private[builder] override def build: Unit = super.build
    }

    class CodeModel(enclosing: Option[ModelElementImpl]) extends ModelElementImpl(enclosing) {

      def owner = {
        assertBuildModelFinished
        __owner
      }

      private var __owner = Option.empty[ModelElementImpl]

      def _owner = __owner

      def _owner_=(modelElement: ModelElementImpl) = {
        assertBuilding()
        assert(__owner isEmpty)
        __owner = Some(this)
      }

      override def build: Unit = super.build
    }


    abstract sealed class ClassLikeImpl (val sym: Symbol, enclosing: Option[ModelElementImpl]) extends ModelElementImpl(enclosing) with ClassLike {
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
