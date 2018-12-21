package scalaclean.model

import scalafix.v1.SemanticDocument

import scala.meta.{Defn, Pkg, Source, Stat}

sealed trait ClassLike {
  def fullName: String
}
sealed trait ClassModel extends ClassLike
sealed trait ObjectModel extends ClassLike
sealed trait TraitModel extends ClassLike

class ScalaCleanModel {
  def printStructure() = allClasses foreach {
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

  lazy val allClasses: Seq[ClassModel] = all collect {
    case cls: ClassModel => cls
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

      val resolved = allKnownObjects.getOrElseUpdate(sym.toString, new ObjectModelImpl(sym))
      resolved.analyse(obj)
      println(s"object = ${resolved.fullName}")
    }

    def visitTrait(obj: Defn.Trait)(implicit doc: SemanticDocument): Unit = {
      val sym = obj.symbol

      val resolved = allKnownTraits.getOrElseUpdate(sym.toString, new TraitModelImpl(sym))
      resolved.analyse(obj)
      println(s"trait = ${resolved.fullName}")
    }

    def visitClass(cls: Defn.Class)(implicit doc: SemanticDocument): Unit = {
      val sym = cls.symbol

      val resolved = allKnownClasses.getOrElseUpdate(sym.toString, new ClassModelImpl(sym))
      resolved.analyse(cls)
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

      private[ScalaCleanModel] val elements = List.newBuilder[ModelElement]

    }


    sealed trait ModelElement {
      assertBuilding
      elements += this

      private[builder] def build: Unit = ()
    }

    class FieldModel(val name: String) extends ModelElement {
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

    class MethodModel(val name: String) extends ModelElement {

      private[builder] override def build: Unit = super.build
    }

    class CodeModel() extends ModelElement {

      def owner = {
        assertBuildModelFinished
        __owner
      }

      private var __owner = Option.empty[ModelElement]

      def _owner = __owner

      def _owner_=(modelElement: ModelElement) = {
        assertBuilding()
        assert(__owner isEmpty)
        __owner = Some(this)
      }

      override def build: Unit = super.build
    }


    abstract sealed class ClassLikeImpl (val sym: Symbol) extends ModelElement {
      lazy val fields: Map[String, FieldModel] = {
        assertBuildModelFinished
        fieldData.toMap
      }
      lazy val methods: Seq[MethodModel] = {
        assertBuildModelFinished
        methodData.result()
      }
      lazy val outerClass: Seq[MethodModel] = {
        assertBuildModelFinished
        methodData.result()
      }
      private val fieldData = mutable.Map[String, FieldModel]()
      private val methodData = List.newBuilder[MethodModel]
      private val innerData = List.newBuilder[ClassLikeImpl]
      private var outerData = Option.empty[ClassLikeImpl]
      private val parentData = List.newBuilder[ClassLikeImpl]
      private val childData = List.newBuilder[ClassLikeImpl]

      private[builder] def addField(field: FieldModel): Unit = {
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

    class ClassModelImpl private[ScalaCleanModel](sym: Symbol) extends ClassLikeImpl(sym) with ClassModel {
      def analyse(cls: Defn.Class)(implicit doc: SemanticDocument): Unit = analyse(cls.templ)
    }

    class ObjectModelImpl private[ScalaCleanModel](sym: Symbol) extends ClassLikeImpl(sym) with ObjectModel {
      def analyse(cls: Defn.Object)(implicit doc: SemanticDocument): Unit = analyse(cls.templ)
    }

    class TraitModelImpl private[ScalaCleanModel](sym: Symbol) extends ClassLikeImpl(sym) with TraitModel {
      def analyse(cls: Defn.Trait)(implicit doc: SemanticDocument): Unit = analyse(cls.templ)
    }

  }

}
