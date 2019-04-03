package scalaclean.model.impl.v1

import java.net.URLClassLoader

import scalaclean.model
import scalaclean.model.{Refers, ScalaCleanModel, Utils}
import scalaclean.model.reflect.GlobalHelper
import scalafix.internal.v1.InternalSemanticDoc
import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.{Decl, Defn, Mod, Pat, Pkg, Source, Stat, Template, Term, Tree, Type}
import scala.reflect.ClassTag
import scala.reflect.runtime.JavaUniverse
import scalaclean.model.impl._

object ModelHooks {
  type ModelElement = ModelElementHook
  type ClassLike = ClassLikeHook
  type ClassModel = ClassModelHook
  type ObjectModel = ObjectModelHook
  type TraitModel = TraitModelHook
  type MethodModel = MethodModelHook
  type FieldModel = FieldModelHook
  type ValModel = ValModelHook
  type VarModel = VarModelHook
}
class ScalaCleanModelImpl extends ScalaCleanModel{
  import ModelHooks._
  def fromSymbol[T <: model.ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): T =
    builder.bySymbol.get(symbol) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x) if tpe.runtimeClass.isInstance(x) => x.asInstanceOf[T]
      case Some(x) => throw new IllegalArgumentException(s"Unexxpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
    }

  def getSymbol[T <: model.ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): Option[T] =
    builder.bySymbol.get(symbol) map {
      case x if tpe.runtimeClass.isInstance(x) => x.asInstanceOf[T]
      case x => throw new IllegalArgumentException(s"Unexxpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
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

  def allOf[T <: model.ModelElement : ClassTag]: List[T] = {
    val cls = implicitly[ClassTag[T]].runtimeClass
    all collect {
      case wanted if cls.isInstance(wanted) => wanted.asInstanceOf[T]
    }
  }

  private object builder {

    import ModelBuilder._
    import scalafix.v1._

    import collection.mutable

    def debug(s: => String) = if (false) println(s)

    val ru = scala.reflect.runtime.universe.asInstanceOf[scala.reflect.runtime.JavaUniverse]
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

    private[this] val cachedClassLoader = new java.util.IdentityHashMap[GlobalSymbolTable, ru.JavaMirror]

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

        private def visitEnclosingChildren(parent: ModelElementImpl, tree: Tree): Unit = {
          visitEnclosingChildren(List(parent), tree)
        }

        private def visitEnclosingChildren(parent: List[ModelElementImpl], tree: Tree): Unit = {
          val prev = enclosing
          enclosing = parent
          visitChildren(tree)
          enclosing = prev
        }

        private def visitChildren(tree: Tree): Unit = {
          tree.children.foreach {
            child =>
              visitTree(child)
          }
        }

        def visitTree(tree: Tree): Unit = {
          val sym = tree.symbol(doc)
          if (!sym.isNone)
            debug(s"${tree.pos.startLine}:${tree.pos.startColumn} - ${tree.pos.endLine}:${tree.pos.endColumn} ${tree.symbol(doc)}")

          tree match {
            case _: Pkg =>
              visitChildren(tree)
            case _: Source =>
              visitChildren(tree)
            case obj: Defn.Object =>
              val sym = obj.symbol
              val parent = allKnownObjects.getOrElseUpdate(sym.toString, new ObjectModelImpl(obj, enclosing, doc))
              debug(s"object = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Class =>
              val sym = cls.symbol
              val parent = allKnownClasses.getOrElseUpdate(sym.toString, new ClassModelImpl(cls, enclosing, doc))
              debug(s"class = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case cls: Defn.Trait =>
              val sym = cls.symbol
              val parent = allKnownTraits.getOrElseUpdate(sym.toString, new TraitModelImpl(cls, enclosing, doc))
              debug(s"trait = ${parent.fullName}")
              visitEnclosingChildren(parent, tree)
            case method: Defn.Def =>
              val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
              val fullSig = s"${method.symbol}:$typeSigs"
              val parent = new MethodModelDefn(method, enclosing, doc)
              visitEnclosingChildren(parent, tree)
            case method: Decl.Def =>
              val typeSigs = method.paramss.map(_.map(v => v.decltpe.get)).toString
              val fullSig = s"${method.symbol}:$typeSigs"
              val parent = new MethodModelDecl(method, enclosing, doc)
              visitEnclosingChildren(parent, tree)
            case valDef: Defn.Val =>
              //to cope with  val (x,Some(y)) = ....
              val fields = Utils.readVars(valDef.pats)
              val fieldModels = fields map {
                new ValModelDefn(valDef, _, fields, enclosing, doc)
              }
              visitEnclosingChildren(fieldModels, tree)
            case valDef: Decl.Val =>
              //to cope with  val (x,Some(y)) = ....
              val fields = Utils.readVars(valDef.pats)
              val fieldModels = fields map {
                new ValModelDecl(valDef, _, fields, enclosing, doc)
              }
              visitEnclosingChildren(fieldModels, tree)
            case varDef: Defn.Var =>
              //to cope with  var (x,Some(y)) = ....
              val fields = Utils.readVars(varDef.pats)
              val fieldModels = fields map {
                new VarModelDefn(varDef, _, fields, enclosing, doc)
              }
              visitEnclosingChildren(fieldModels, tree)
            case varDef: Decl.Var =>
              //to cope with  var (x,Some(y)) = ....
              val fields = Utils.readVars(varDef.pats)
              val fieldModels = fields map {
                new VarModelDecl(varDef, _, fields, enclosing, doc)
              }
              visitEnclosingChildren(fieldModels, tree)
            case _ =>
              val thisTreeSymbol = tree.symbol
              foundSymbol(tree.symbol, tree)
              visitChildren(tree)
          }
          //and synthetics
          tree match {
            case term: Term =>
              term.synthetic foreach (visitSynthetic(_, term))
            case _ => //ignore
          }
        }

        def foundSymbol(symbol: Symbol, tree: Tree): Unit = {
          if (!symbol.isNone)
            if (enclosing.isEmpty) {
              debug(s"cant add to parent  ${tree.getClass} ${tree.pos.start} .. ${tree.pos.end} synthetic:$inSynthetic - ${symbol}")
            } else {
              if (enclosing.forall(_.symbol != symbol))
                enclosing foreach {
                  _.addRefersTo(tree, symbol, inSynthetic)
                }
            }
        }

        var inSynthetic = false

        def visitSynthetic(tree: SemanticTree, orig: Tree): Unit = {
          val wasInSynthetic = inSynthetic
          inSynthetic = true
          tree match {
            case NoTree =>
            case LiteralTree(constant: Constant) =>

            case IdTree(info: SymbolInformation) =>
              foundSymbol(info.symbol, orig)

            case SelectTree(qualifier: SemanticTree, id: IdTree) =>
              visitSynthetic(qualifier, orig)
              visitSynthetic(id, orig)

            case ApplyTree(function: SemanticTree, arguments: List[SemanticTree]) =>
              visitSynthetic(function, orig)
              arguments foreach (visitSynthetic(_, orig))

            case TypeApplyTree(function: SemanticTree, typeArguments: List[SemanticType]) =>
              visitSynthetic(function, orig)
              typeArguments foreach (visitType(_, orig))

            case FunctionTree(parameters: List[IdTree], body: SemanticTree) =>
              parameters foreach (visitSynthetic(_, orig))
              visitSynthetic(body, orig)

            case MacroExpansionTree(beforeExpansion: SemanticTree, tpe: SemanticType) =>
              visitSynthetic(beforeExpansion, orig)
              visitType(tpe, orig)

            case OriginalSubTree(tree: scala.meta.Tree) =>
            // we don't visit the original tree - that how we got here
            //  so we have traversed it already
            // or will when we return the the tree traversal
            // visitTree(tree)
            case OriginalTree(tree: scala.meta.Tree) =>
            // we don't visit the original tree - that how we got here
            //  so we have traversed it already
            // or will when we return the the tree traversal
            // visitTree(tree)
          }
          inSynthetic = wasInSynthetic
        }

        def visitType(tpe: SemanticType, tree: Tree): Unit = tpe match {

          case TypeRef(prefix: SemanticType, symbol: Symbol, typeArguments: List[SemanticType]) =>
            visitType(prefix, tree)
            foundSymbol(symbol, tree)
            typeArguments foreach {
              visitType(_, tree)
            }
          case SingleType(prefix: SemanticType, symbol: Symbol) =>
            visitType(prefix, tree)
            foundSymbol(symbol, tree)
          case ThisType(symbol: Symbol) =>
            foundSymbol(symbol, tree)
          case SuperType(prefix: SemanticType, symbol: Symbol) =>
            visitType(prefix, tree)
            foundSymbol(symbol, tree)
          case ConstantType(constant: Constant) =>
          case IntersectionType(types: List[SemanticType]) =>
            types foreach {
              visitType(_, tree)
            }
          case UnionType(types: List[SemanticType]) =>
            types foreach {
              visitType(_, tree)
            }
          case WithType(types: List[SemanticType]) =>
            types foreach {
              visitType(_, tree)
            }
          case StructuralType(tpe: SemanticType, declarations: List[SymbolInformation]) =>
            visitType(tpe, tree)
            //not sure it it worth visiting these
            declarations foreach {
              i =>
                foundSymbol(i.symbol, tree)
            }
          case AnnotatedType(annotations: List[Annotation], tpe: SemanticType) =>
            annotations foreach { a => visitType(a.tpe, tree) }
            visitType(tpe, tree)
          case ExistentialType(tpe: SemanticType, declarations: List[SymbolInformation]) =>
            visitType(tpe, tree)
            declarations foreach {
              i =>
                foundSymbol(i.symbol, tree)
            }
          case UniversalType(typeParameters: List[SymbolInformation], tpe: SemanticType) =>
            typeParameters foreach {
              i =>
                foundSymbol(i.symbol, tree)
            }
            visitType(tpe, tree)
          case ByNameType(tpe: SemanticType) =>
            visitType(tpe, tree)
          case RepeatedType(tpe: SemanticType) =>
            visitType(tpe, tree)
          case NoType =>
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

      private[ScalaCleanModelImpl] val elements = List.newBuilder[ModelElementImpl]

    }


    sealed abstract class ModelElementImpl(protected val stat: Stat, val enclosing: List[ModelElementImpl], protected val doc: SemanticDocument) extends ModelElement {
      assertBuilding()
      assert(!symbol.isNone)
      assert(bySymbol.put(symbol, this).isEmpty, s"$symbol enclosing $enclosing this =$this")

      def addRefersTo(tree: Tree, symbol: Symbol, isSynthetic: Boolean): Unit = {
        _refersTo ::= new RefersImpl(this.symbol, symbol, isSynthetic)
      }

      override protected def infoPosString: String = {
        val pos = stat.pos
        s"${pos.startLine}:${pos.startColumn} - ${pos.endLine}:${pos.endColumn}"
      }

      private var _refersTo = List.empty[Refers]
      private var _refersFrom = List.empty[(ModelElementImpl, Refers)]

      assertBuilding
      elements += this

      override final def fields: List[FieldModel] = {
        assertBuildModelFinished

        children.collect {
          case field: FieldModel => field
        }
      }

      override final def methods: List[MethodModel] = {
        assertBuildModelFinished
        children collect {
          case m: MethodModel => m
        }
      }

      override final def innerClassLike: Seq[ClassLike] = {
        assertBuildModelFinished
        children collect {
          case m: ClassModel => m
        }
      }


      private[builder] def build: Unit = {
        _refersTo foreach {
          case ref: Refers=>
            val symToRef = bySymbol.get(ref.toSymbol)
            symToRef foreach {
              _._refersFrom ::= (this, ref)
            }
        }
        _directOverrides flatMap bySymbol.get foreach {
          _._directOverridden ::= this
        }
        _transitiveOverrides flatMap bySymbol.get foreach {
          _._transitiveOverridden ::= this
        }

      }


      override def symbolInfo: SymbolInformation = doc.info(symbol).get

      override def symbolInfo(anotherSymbol: Symbol): SymbolInformation = doc.info(anotherSymbol).get

      override def internalOutgoingReferences: List[(ModelElementImpl, Refers)] = {
        assertBuildModelFinished
        for (refersTo <- _refersTo;
             ref <- bySymbol.get(refersTo.toSymbol)) yield {
          (ref, refersTo)
        }
      }

      override def allOutgoingReferences: List[(Option[ModelElementImpl], Refers)] = {
        assertBuildModelFinished
        for (refersTo <- _refersTo) yield {
          (bySymbol.get(refersTo.toSymbol), refersTo)
        }
      }

      override def internalIncomingReferences: List[(ModelElementImpl, Refers)] = {
        assertBuildModelFinished
        _refersFrom
      }

      private var _directOverrides = List.empty[Symbol]
      private var _transitiveOverrides = List.empty[Symbol]
      private var _directOverridden = List.empty[ModelElementImpl]
      private var _transitiveOverridden = List.empty[ModelElementImpl]

      override def allDirectOverrides: List[(Option[ModelElement], Symbol)] = {
        assertBuildModelFinished()
        _directOverrides map {
          sym => (bySymbol.get(sym), sym)
        }
      }

      override def internalDirectOverrides: List[ModelElement] = {
        assertBuildModelFinished()
        _directOverrides flatMap bySymbol.get
      }

      override def allTransitiveOverrides: List[(Option[ModelElement], Symbol)] = {
        assertBuildModelFinished()
        _transitiveOverrides map {
          sym => (bySymbol.get(sym), sym)
        }
      }

      override def internalTransitiveOverrides: List[ModelElement] = {
        assertBuildModelFinished()
        _transitiveOverrides flatMap bySymbol.get
      }


      override def internalDirectOverriddenBy: List[ModelElement] = {
        assertBuildModelFinished()
        _directOverridden
      }

      override def internalTransitiveOverriddenBy: List[ModelElement] = {
        assertBuildModelFinished()
        _transitiveOverridden
      }

      protected def recordOverrides(directOverides: List[Symbol], transitiveOverrides: List[Symbol]) = {
        assertBuilding()
        assert(_directOverrides eq Nil)
        assert(_transitiveOverrides eq Nil)
        _directOverrides = directOverides.distinct
        _transitiveOverrides = transitiveOverrides.distinct
      }

      enclosing foreach {
        _._children ::= this
      }
      private var _children = List.empty[ModelElementImpl]

      private[ScalaCleanModelImpl] def children = _children

      override def symbol: Symbol = stat.symbol(doc)

      def name = symbol.displayName

      def recordInheritance(localSymbols: List[ru.Symbol]): Unit = {
        val direct = List.newBuilder[ru.Symbol]
        val all = List.newBuilder[ru.Symbol]
        localSymbols foreach {
          sym =>
            val thisType = sym.owner.thisType
            val builder = List.newBuilder[ru.Symbol]
            for (parent <- sym.owner.ancestors) {
              val parentOveridden = sym.matchingSymbol(parent, thisType)
              if (parentOveridden ne ru.NoSymbol) builder += parentOveridden
            }
            val allParents = builder.result()
            allParents.headOption foreach (direct += _)
            all ++= allParents
        }
        val directSyms = direct.result map (rsym => Symbol(globalHelper.gSymToMSymString(rsym)))
        val allSyms = all.result map (rsym => Symbol(globalHelper.gSymToMSymString(rsym)))
        if (directSyms.mkString contains "<no symbol>") {
          println("")
        }
        recordOverrides(directSyms, allSyms)
      }

      def recordFieldOverrides(fieldType: String, name: String) = {

        //record overrides
        //for a method or field to override it must have a parent which is a class/trait

        enclosing.headOption match {
          case Some(cls: ClassLikeImpl) =>
            val sym = cls.reflectSymbol
            val wanted = s"$fieldType ${name}"
            val found = sym.toType.decls.toList.filter {
              _.toString == wanted
              //            case sym: JavaUniverse#TermSymbol => sym.keyString == keyString && sym.unexpandedName.getterName.decode.toString == field.name.value
              //            case _ => false
            }
            recordInheritance(found)

          case _ => // local cant override

        }
      }

      override def classOrEnclosing: ClassLike
    }

    abstract sealed class FieldModelImpl(defn: Stat, field: Pat.Var, allFields: Seq[Pat.Var],
                                         enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      ModelElementImpl(defn, enclosing, doc) with FieldModel {
      self: FieldModel =>
      require(allFields.nonEmpty)

      override def symbol: Symbol = field.symbol(doc)

      override def infoName: String = field.name.toString

      def asField: FieldModel = this

      def isAbstract: Boolean

      final override def otherFieldsInSameDeclaration: Seq[fieldType] =
        (allFields.filter(_ == field)) map (f => bySymbol(f.symbol(doc)).asInstanceOf[fieldType])

      override def classOrEnclosing: ClassLike = {
        enclosing.head.classOrEnclosing
      }
    }

    abstract class VarModelImpl(vr: Stat, field: Pat.Var, allFields: Seq[Pat.Var],
                                enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      FieldModelImpl(vr, field, allFields, enclosing, doc) with VarModel {
      recordFieldOverrides("variable", field.name.value)
    }

    class VarModelDefn(vr: Defn.Var, field: Pat.Var, allFields: Seq[Pat.Var],
                       enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      VarModelImpl(vr, field, allFields, enclosing, doc) {

      override def isAbstract: Boolean = false
    }

    class VarModelDecl(vr: Decl.Var, field: Pat.Var, allFields: Seq[Pat.Var],
                       enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      VarModelImpl(vr, field, allFields, enclosing, doc) {

      override def isAbstract: Boolean = true
    }

    abstract class ValModelImpl(vl: Stat, field: Pat.Var, allFields: Seq[Pat.Var],
                                enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      FieldModelImpl(vl, field, allFields, enclosing, doc) with ValModel {
      recordFieldOverrides("value", field.name.value)

      override protected def infoDetail: String = s"lazy=$isLazy"

      override def isLazy: Boolean = val_mods.exists(_.isInstanceOf[Mod.Lazy])

      def val_mods: List[Mod]
    }

    class ValModelDefn(val valDef: Defn.Val, field: Pat.Var, allFields: Seq[Pat.Var],
                       enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      ValModelImpl(valDef, field, allFields, enclosing, doc) {
      override def val_mods = valDef.mods

      override def isAbstract: Boolean = false
    }

    class ValModelDecl(val valDecl: Decl.Val, field: Pat.Var, allFields: Seq[Pat.Var],
                       enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      ValModelImpl(valDecl, field, allFields, enclosing, doc) {
      override def val_mods = valDecl.mods

      override def isAbstract: Boolean = true
    }

    abstract class MethodModelImpl(stat: Stat, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends
      ModelElementImpl(stat, enclosing, doc) with MethodModel {

      private def paramsMatch(gSymParamss: List[List[JavaUniverse#Symbol]], metaParamss: List[List[Term.Param]]): Boolean = {
        gSymParamss.size == metaParamss.size &&
          (gSymParamss zip metaParamss forall {
            case (gSymParams: List[JavaUniverse#Symbol], metaParams: List[Term.Param]) =>
              gSymParams.length == metaParams.length &&
                (gSymParams zip metaParams forall {
                  case (gSymParam: JavaUniverse#Symbol, metaParam: Term.Param) =>
                    assert(metaParam.decltpe.isDefined)
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
          val sym = cls.reflectSymbol
          val list = sym.toType.decls.toList
          val simpleMethodMatch = list.collect {
            case sym: ru.MethodSymbol
              if !sym.isClassConstructor
                && sym.nameString == method_name.toString => sym
          }
          val found = simpleMethodMatch filter {
            case sym =>
              val symParams = sym.paramLists
              val defnParams = method_paramss
              (symParams, defnParams) match {
                case (Nil, List(Nil)) => true
                case (List(Nil), Nil) => true
                case _ => paramsMatch(symParams, defnParams)
              }
          }
          assert(found.size == 1, s"could not match the method ${stat} from $simpleMethodMatch - found=$found - orig = $list")
          recordInheritance(found)
        case _ => // local cant override

      }

      def method_mods: List[Mod]

      def method_name: Term.Name

      def method_tparams: List[Type.Param]

      def method_paramss: List[List[Term.Param]]

      def method_decltpe: Option[Type]

      def isAbstract: Boolean

      def paramsType = method_paramss map (_.map {
        param: Term.Param => param.symbol(doc)
      })

      override def classOrEnclosing: ClassLike = enclosing.head.classOrEnclosing
    }

    class MethodModelDefn(val defn: Defn.Def, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends MethodModelImpl(defn, enclosing, doc) {
      def method_mods = defn.mods

      def method_name = defn.name

      def method_tparams = defn.tparams

      def method_paramss = defn.paramss

      def method_decltpe = defn.decltpe

      def isAbstract = false
    }

    class MethodModelDecl(val decl: Decl.Def, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends MethodModelImpl(decl, enclosing, doc) {
      def method_mods = decl.mods

      def method_name = decl.name

      def method_tparams = decl.tparams

      def method_paramss = decl.paramss

      def method_decltpe = Some(decl.decltpe)

      def isAbstract = true
    }

    abstract sealed class ClassLikeImpl(defn: Defn, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ModelElementImpl(defn, enclosing, doc) with ClassLike {
      private var _directExtendedBy = Set.empty[model.ClassLike]

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
        directExtends.foldLeft(directExtends) {
          case (result: Set[Symbol], direct) =>
            result ++ xtends(direct)
        }
      }

      override def directExtendedBy: Set[model.ClassLike] = {
        assertBuildModelFinished()
        _directExtendedBy
      }

      override def transitiveExtendedBy: Set[model.ClassLike] = {
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
        xtends(Symbol(cls.runtimeClass.getName.replace('.', '/') + "#"))
      }

      override def xtends(symbol: Symbol): Boolean = {
        transitiveExtends.contains(symbol)
      }

      def reflectSymbol = {
        val jm = mirror(doc)
        //.asInstanceOf[JavaUniverse#Mirror]
        val javaName = fullName.
          substring(0, fullName.length - 1).
          replace('#', '.').
          replace('.', '.').
          replace('/', '.')
        val res = if (this.isInstanceOf[ObjectModelImpl]) jm.getRequiredModule(javaName)
        else jm.getRequiredClass(javaName)
        res
      }

      override def classOrEnclosing: ClassLike = this
    }

    class ClassModelImpl private[ScalaCleanModelImpl](cls: Defn.Class, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ClassModel {
      override protected def template: Template = cls.templ
    }

    class ObjectModelImpl private[ScalaCleanModelImpl](cls: Defn.Object, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with ObjectModel {
      override protected def template: Template = cls.templ

      recordFieldOverrides("object", cls.name.value)
    }

    class TraitModelImpl private[ScalaCleanModelImpl](cls: Defn.Trait, enclosing: List[ModelElementImpl], doc: SemanticDocument) extends ClassLikeImpl(cls, enclosing, doc) with TraitModel {
      override protected def template: Template = cls.templ
    }

  }

}