package scalaclean.model

import scalafix.v1.SemanticDocument

import scala.meta.{Defn, Pkg, Source, Stat}

sealed trait ClassModel {
  def fullName: String
}

package analysis {
  sealed trait AnalysisClassModel extends ClassModel{
    def analyse(cls: Defn.Class)(implicit doc: SemanticDocument)
  }

}




class ScalaCleanModel {
  def printStructure() = allClasses foreach {
    cls => println(s"class ${cls.fullName}")
  }

  def analyse(implicit doc: SemanticDocument) = builder.analyse


  import builder._
  import analysis._

  def getOrCreateClass(cls: Defn.Class)(implicit doc: SemanticDocument): AnalysisClassModel = {
    import scalafix.v1._

    ClassModelImpl.getOrCreate(cls.name.symbol)
  }

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


    def visitPkgStatements(pkg: Pkg, statements: List[Stat])(implicit doc: SemanticDocument): Unit = {
      println(s"Package: ${pkg.symbol}")
      statements.foreach(visitPkgStatement(_))
    }

    def visitObject(obj: Defn.Object)(implicit doc: SemanticDocument): Unit = {
      println(s"Object = ${obj.symbol}")

    }

    def visitClass(cls: Defn.Class, outerClass: Option[AnalysisClassModel])(implicit doc: SemanticDocument): Unit = {
      val scCls = getOrCreateClass(cls)
      scCls.analyse(cls)
      println(s"class = ${scCls.fullName}")
    }

    def visitPkgStatement(statement: Stat)(implicit doc: SemanticDocument): Unit = {
      statement match {
        case childPkg@Pkg(pName, pstats) => // a package containing a sub-package
          visitPkgStatements(childPkg, pstats)
        case o: Defn.Object =>
          visitObject(o)
        case c: Defn.Class =>
          visitClass(c, None)
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

      private[model] val elements = List.newBuilder[ModelElement]

    }


    sealed trait ModelElement {
      assertBuilding
      elements += this

      private[model] def build: Unit = ()
    }

    class FieldModel(val name: String) extends ModelElement {
      def initialiser = {
        assertBuildModelFinished
        _initialiser
      }

      private var _initialiser = Option.empty[CodeModel]

      private[model] def initialiser_=(codeModel: CodeModel): Unit = {
        assertBuilding
        assert(_initialiser isEmpty)
        assert(codeModel._owner isEmpty)
        codeModel._owner = this
        _initialiser = Some(codeModel)
      }

      private[model] override def build: Unit = super.build

    }

    class MethodModel(val name: String) extends ModelElement {

      private[model] override def build: Unit = super.build
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

    object ClassModelImpl {
      val allKnown = mutable.Map[String, ClassModelImpl]()

      def getOrCreate(name: Symbol): ClassModelImpl = {
        allKnown.getOrElseUpdate(name.toString, new ClassModelImpl(name))
      }
    }

    class ClassModelImpl private(val sym: Symbol) extends ModelElement with AnalysisClassModel {

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
      private val innerClassData = List.newBuilder[ClassModelImpl]
      private var outerClassData = Option.empty[ClassModelImpl]
      private val parentClassData = List.newBuilder[ClassModelImpl]
      private val childClassData = List.newBuilder[ClassModelImpl]

      private[model] def addField(field: FieldModel): Unit = {
        assertBuilding
        assert(!fieldData.contains(field.name))
        fieldData(field.name) = field
      }

      private[model] def addInnerClass(c: ClassModelImpl): Unit = {
        assertBuilding
        assert(c.outerClassData.isEmpty)
        c.outerClassData = Some(this)
        innerClassData += c
      }

      private[model] def addParentClass(c: ClassModelImpl): Unit = {
        assertBuilding
        parentClassData += c
        c.childClassData += this
      }


      override def analyse(cls: Defn.Class)(implicit doc: SemanticDocument): Unit = {
        cls.templ.stats.foreach {
          case vl: Defn.Val =>
          case vr: Defn.Var =>
          case df@Defn.Def(mods, defName, _, _, _, _) =>
            println(s"  method = $defName  " + mods.structureLabeled)

          case dc: Defn.Class => // Inner class

        }

      }

      override def fullName: String = sym.toString

      private[model] override def build: Unit = super.build
    }

  }

}