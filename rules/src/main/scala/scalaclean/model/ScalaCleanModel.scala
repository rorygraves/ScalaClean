package scalaclean.model

import java.net.URLClassLoader

import scalaclean.model.reflect.GlobalHelper
import scalafix.internal.v1.InternalSemanticDoc
import scalafix.v1.{SemanticDocument, Symbol, SymbolInformation}

import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.{Defn, Pat, Pkg, Source, Template, Tree, Term}
import scala.reflect.ClassTag
import scala.reflect.runtime.JavaUniverse


sealed trait ModelElement {
  def symbol: Symbol

  var colour : Colour = _
  def name: String

  //usually just one element. Can be >1 for  RHS of a val (a,b,c) = ...
  //where a,b,c are the enclosing
  def enclosing: List[ModelElement]

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

    val ru = scala.reflect.runtime.universe
    val globalHelper = new GlobalHelper(ru.asInstanceOf[scala.reflect.runtime.JavaUniverse])
    val allKnownClasses = mutable.Map[String, ClassModelImpl]()
    val allKnownObjects = mutable.Map[String, ObjectModelImpl]()
    val allKnownTraits = mutable.Map[String, TraitModelImpl]()
    val bySymbol = mutable.Map[Symbol, ModelElementImpl]()

    private var enclosing = List.empty[ModelElementImpl]
    private object access {
      val internalAccess = classOf[SemanticDocument].getMethod("internal")
      val classpathAccess = classOf[GlobalSymbolTable].getDeclaredField("classpath")
      classpathAccess.setAccessible(true)
    }
    def internalDoc(doc: SemanticDocument) =
      access.internalAccess.invoke(doc).asInstanceOf[InternalSemanticDoc]
    def symbolTable(doc: SemanticDocument) =
      internalDoc(doc).symtab
    private [this] val cachedClassLoader = new java.util.IdentityHashMap[GlobalSymbolTable, ru.JavaMirror]

    def mirror(doc: SemanticDocument) = {
      val symbolTable = internalDoc(doc).symtab.asInstanceOf[GlobalSymbolTable]
      cachedClassLoader.computeIfAbsent(symbolTable, {
        symbolTable =>

          import scala.meta.io._
          val classpath = access.classpathAccess.get(symbolTable).asInstanceOf[Classpath]
          val urls = classpath.shallow.map {
            path => path.toURI.toURL
          }
          val cl = new URLClassLoader(urls.toArray)
          ru.runtimeMirror(cl)
      })
    }

    def analyse(implicit doc: SemanticDocument) = {
      assertBuilding()
      object analysisVisitor {

        final def visitDocument(tree: Tree): Unit = {
          visitTree(tree)
        }

        private def visitEnclosingChildren(parent: ModelElementImpl, t: Tree): Unit = {
          visitEnclosingChildren(List(parent), t)
        }
        private def visitEnclosingChildren(parent: List[ModelElementImpl], t: Tree): Unit = {
          val prev = enclosing
          enclosing = parent
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
            case _: Pkg =>
              visitChildren(tree)
            case _: Source =>
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
              //to cope with  val (x,Some(y)) = ....
              val fields = readVars(valDef.pats)
              val fieldModels = fields map {
                new ValModelImpl(valDef, _, enclosing, doc)
              }
              visitEnclosingChildren(fieldModels, tree)
            case varDef: Defn.Var =>
              //to cope with  var (x,Some(y)) = ....
              val fields = readVars(varDef.pats)
              val fieldModels = fields map {
                new VarModelImpl(varDef, _, enclosing, doc)
              }
              visitEnclosingChildren(fieldModels, tree)
            case _ =>
              if (!tree.symbol.isNone)
                if (enclosing.isEmpty) {
                  val pos = tree.pos
                  println(s"XXX cant add to parent = ${tree.getClass} ${pos.start} .. ${pos.end} - ${tree.symbol}")
                } else {
                  enclosing foreach {
                    _.addRefersTo(tree)
                  }
                }
              visitChildren(tree)
          }
        }
      }
      analysisVisitor.visitDocument(doc.tree)

      def readVars(pats: List[Pat]): List[Pat.Var] = {
        object visitor {
          var res = List.empty[Pat.Var]

          def visitTree(tree: Tree): Unit = {
            tree match {
              case field: Pat.Var =>
                res ::= field
              case _ =>
            }
            tree.children.foreach {visitTree(_)}
          }
        }
        pats foreach visitor.visitTree
        assert (visitor.res.nonEmpty)
        visitor.res
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


    sealed abstract class ModelElementImpl(protected val defn: Defn, val enclosing: List[ModelElementImpl], protected val doc: SemanticDocument) extends ModelElement {
      assertBuilding()
      assert(!symbol.isNone)
      assert(bySymbol.put(symbol, this).isEmpty, s"$symbol enclosing $enclosing this =$this")

      def addRefersTo(tree: Tree): Unit = {
        if (tree.symbol(doc) != symbol)
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

      //TODO detect override and update _directOverrides && _directOverrided

      enclosing foreach {
        _._children ::= this
      }
      private var _children = List.empty[ModelElementImpl]

      private[ScalaCleanModel] def children = _children

      override def symbol: Symbol = defn.symbol(doc)

      def name = symbol.displayName
      def recordFieldOverrides(fieldType: String, name: String) = {

        //record overrides
        //for a method or field to override it must have a parent which is a class/trait

        enclosing.headOption match {
          case Some(cls: ClassLikeImpl) =>
            val sym = cls.relectSymbol
            val wanted = s"$fieldType ${name}"
            val found = sym.toType.decls.toList.filter {
              _.toString == wanted
              //            case sym: JavaUniverse#TermSymbol => sym.keyString == keyString && sym.unexpandedName.getterName.decode.toString == field.name.value
              //            case _ => false
            }
            found foreach {
              o =>
                val overrides = o.overrides
                println(s"$o overrides ${overrides}")
                overrides foreach {
                  parent =>
                    val metaSymbolString = globalHelper.gSymToMSymString(parent)
                    recordOverrides(Symbol(metaSymbolString))
                }
            }
          case _ => // local cant override

        }
      }

    }

    abstract class FieldModelImpl(defn: Defn, field: Pat.Var, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(defn, enclosing, doc) with FieldModel {


      override def symbol: Symbol = field.symbol(doc)
    }

    class VarModelImpl(vr: Defn.Var, field: Pat.Var, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends FieldModelImpl(vr, field, enclosing, doc) with VarModel {
      recordFieldOverrides("variable", field.name.value)
    }

    class ValModelImpl(vl: Defn.Val, field: Pat.Var, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends FieldModelImpl(vl, field, enclosing, doc) with ValModel {
      recordFieldOverrides("value", field.name.value)
    }

    class MethodModelImpl(df: Defn.Def, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(df, enclosing, doc) with MethodModel {
      private[builder] override def build: Unit = super.build

      private def paramsMatch (gSymParamss: List[List[JavaUniverse#Symbol]],metaParamss:  List[List[Term.Param]]) : Boolean = {
        gSymParamss.size == metaParamss.size &&
          (gSymParamss zip metaParamss forall {
            case (gSymParams: List[JavaUniverse#Symbol], metaParams: List[Term.Param]) =>
              gSymParams.length == metaParams.length &&
                (gSymParams zip metaParams forall {
                  case (gSymParam: JavaUniverse#Symbol, metaParam: Term.Param) =>
                    assert (metaParam.decltpe.isDefined)
                    //TODO not a great compare
                    gSymParam.tpe.toString == metaParam.decltpe.get.toString
                })
          })
      }

      //record overrides
      //for a method to override it must have a parent which is a class/trait
      //so no locals

      //TODO consider varargs
      //TODO can we put this in scalafix without the hop to reflection

      enclosing.headOption match {
        case Some(cls: ClassLikeImpl) =>
          val sym = cls.relectSymbol
          val wanted = s"method ${name}"
          val list = sym.toType.decls.toList
          val simpleMethodMatch = list.collect {
            case sym: JavaUniverse#MethodSymbol
              if !sym.isClassConstructor
                && sym.name.toString == df.name.toString => sym
          }
          val found = simpleMethodMatch filter {
            case sym => paramsMatch(sym.paramLists, df.paramss)
          }
          assert (found.size == 1, s"could not match the method ${df} from $simpleMethodMatch")
          found foreach {
            o =>
              val overrides = o.overrides
              println(s"$o overrides ${overrides}")
              overrides foreach {
                parent =>
                  val metaSymbolString = globalHelper.gSymToMSymString(parent)
                  recordOverrides(Symbol(metaSymbolString))
              }
          }
        case _ => // local cant override

      }
    }

    abstract sealed class ClassLikeImpl(defn: Defn, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(defn, enclosing, doc) with ClassLike {
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
      def relectSymbol = {
        val jm = mirror(doc).asInstanceOf[JavaUniverse#Mirror]
        val javaName = fullName.
          substring(0, fullName.length-1).
          replace('#','$').
          replace('.','$').
        replace('/','.')
        val res: JavaUniverse#Symbol = if (this.isInstanceOf[ObjectModelImpl]) jm.getRequiredModule(javaName)
        else jm.getRequiredClass(javaName)
        res
      }
    }

    class ClassModelImpl private[ScalaCleanModel](cls: Defn.Class, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ClassModel {
      override protected def template: Template = cls.templ
    }

    class ObjectModelImpl private[ScalaCleanModel](cls: Defn.Object, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ObjectModel{
      override protected def template: Template = cls.templ
      recordFieldOverrides("object", cls.name.value)
    }

    class TraitModelImpl private[ScalaCleanModel](cls: Defn.Trait, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with TraitModel{
      override protected def template: Template = cls.templ
    }

  }

}