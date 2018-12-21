package scalaclean.model

import scalafix.v1.SemanticDocument

import scala.meta.{Defn, Pkg, Source, Stat}
import scala.reflect.ClassTag

sealed trait ModelElement{
  var colours = List.empty[Colour]
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

    import collection.mutable
    import ModelBuilder._
    import scalafix.v1._
    import scalafix.v1._


    val allKnownClasses = mutable.Map[String, ClassModelImpl]()
    val allKnownObjects = mutable.Map[String, ObjectModelImpl]()
    val allKnownTraits = mutable.Map[String, TraitModelImpl]()

    def visitPkgStatements(pkg: Pkg, statements: List[Stat])(implicit doc: SemanticDocument): Unit = {
      println(s"Package: ${pkg.symbol}")
      statements.foreach(visitPkgStatement(_))
    }

    def visitObject(obj: Defn.Object)(implicit doc: SemanticDocument): Unit = {
      val sym = obj.symbol

      val resolved = allKnownObjects.getOrElseUpdate(sym.toString, new ObjectModelImpl(sym, obj))
      resolved.analyse
      println(s"object = ${resolved.fullName}")
    }

    def visitTrait(trt: Defn.Trait)(implicit doc: SemanticDocument): Unit = {
      val sym = trt.symbol

      val resolved = allKnownTraits.getOrElseUpdate(sym.toString, new TraitModelImpl(sym, trt))
      resolved.analyse
      println(s"trait = ${resolved.fullName}")
    }

    def visitClass(cls: Defn.Class)(implicit doc: SemanticDocument): Unit = {
      val sym = cls.symbol

      val resolved = allKnownClasses.getOrElseUpdate(sym.toString, new ClassModelImpl(sym, cls))
      resolved.analyse
      println(s"class = ${resolved.fullName}")
    }

    def visitPkgStatement(statement: Stat)(implicit doc: SemanticDocument): Unit = {
      statement match {
        case childPkg@Pkg(pName, pstats) => // a package containing a sub-package
          visitPkgStatements(childPkg, pstats)
        case o: Defn.Object =>
          visitObject(o)
        case c: Defn.Class =>
          visitClass(c)
        case _ =>
          throw new IllegalStateException()
      }
    }

    def analyse(implicit doc: SemanticDocument) = {
      assertBuilding()
      doc.tree match {
        case Source(stats) =>
          stats.foreach(visitPkgStatement(_))
        case _ =>
          throw new IllegalStateException(s"document: ${doc.input} does not start with a Source")
      }
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


    sealed trait ModelElementImpl extends  ModelElement{
      assertBuilding
      elements += this

      private[builder] def build: Unit = ()
    }

    abstract class FieldModelImpl(val name: String) extends ModelElementImpl with FieldModel {
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
    class VarModelImpl(name: String) extends FieldModelImpl(name) with VarModel
    class ValModelImpl(name: String) extends ModelElementImpl with ValModel

    class MethodModelImpl(val name: String) extends ModelElementImpl with MethodModel {

      private[builder] override def build: Unit = super.build
    }

    class CodeModel() extends ModelElementImpl {

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


    abstract sealed class ClassLikeImpl (val sym: Symbol) extends ClassLike with ModelElementImpl {
      lazy val fields: Map[String, FieldModel] = {
        assertBuildModelFinished
        fieldData.toMap
      }
      lazy val methods: List[MethodModel] = {
        assertBuildModelFinished
        methodData.result()
      }
      lazy val outerClass: Seq[MethodModel] = {
        assertBuildModelFinished
        methodData.result()
      }
      private val fieldData = mutable.Map[String, FieldModelImpl]()
      private val methodData = List.newBuilder[MethodModel]
      private val innerData = List.newBuilder[ClassLikeImpl]
      private var outerData = Option.empty[ClassLikeImpl]
      private val parentData = List.newBuilder[ClassLikeImpl]
      private val childData = List.newBuilder[ClassLikeImpl]

      private[builder] def addField(field: FieldModelImpl): Unit = {
        assertBuilding
        assert(!fieldData.contains(field.name))
        fieldData(field.name) = field
      }

      private[builder] def addInner(c: ClassLikeImpl): Unit = {
        assertBuilding
        assert(c.outerData.isEmpty)
        c.outerData = Some(this)
        innerData += c
      }

      private[builder] def addParentClass(c: ClassLikeImpl): Unit = {
        assertBuilding
        parentData += c
        c.childData += this
      }

      def fullName: String = sym.toString

      def analyse(templ: scala.meta.Template)(implicit doc: SemanticDocument): Unit = {
        templ.stats.foreach {
          case vl: Defn.Val =>
          case vr: Defn.Var =>
          case df@Defn.Def(mods, defName, _, _, _, _) =>
            println(s"  method = $defName  " + mods.structureLabeled)

          case dc: Defn.Class => // Inner class
        }
      }
      private[builder] override def build: Unit = ()
    }

    class ClassModelImpl private[ScalaCleanModel](sym: Symbol, cls: Defn.Class) extends ClassLikeImpl(sym) with ClassModel {
      def analyse(implicit doc: SemanticDocument): Unit = analyse(cls.templ)
    }

    class ObjectModelImpl private[ScalaCleanModel](sym: Symbol, cls: Defn.Object) extends ClassLikeImpl(sym) with ObjectModel {
      def analyse(implicit doc: SemanticDocument): Unit = analyse(cls.templ)
    }

    class TraitModelImpl private[ScalaCleanModel](sym: Symbol, trt: Defn.Trait) extends ClassLikeImpl(sym) with TraitModel {
      def analyse(implicit doc: SemanticDocument): Unit = analyse(trt.templ)
    }

  }

}
