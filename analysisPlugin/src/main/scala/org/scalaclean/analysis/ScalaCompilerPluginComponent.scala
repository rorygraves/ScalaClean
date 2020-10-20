package org.scalaclean.analysis

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.Properties

import org.scalaclean.analysis.plugin.ExtensionPlugin
import scalaclean.model.ElementId

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.reflect.internal.Flags
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}

class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with ModelSymbolBuilder {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"

  override val runsAfter: List[String] = List("typer")
  // a bit ugly, but the options are read after the component is create - so it is updated by the plugin
  var debug                    = false
  var _sourceDirs: List[String] = List.empty
  var extensions               = Set.empty[ExtensionPlugin]
  var options: List[String]    = Nil


  override def sourceDirs: List[String] = {
    if (_sourceDirs.isEmpty) {
      val dirs = global.currentRun.compiledFiles.toSet.map{p: String => Paths.get(p).getParent}

      val common: Path = dirs.foldLeft(dirs.head) {
        case (curr, next) =>
          def commonParent(path: Path): Path = if (next.startsWith(path)) path else commonParent(path.getParent)

          commonParent(curr)
      }
      import collection.JavaConverters._
      val scalaDir = common.iterator().asScala.find(path => path.endsWith("scala") && path.getParent.getParent.endsWith("src"))
      assert(scalaDir.nonEmpty, s"can't determine root source dir from files ${ global.currentRun.compiledFiles} ")
      _sourceDirs = scalaDir.toList.map(_.toString)
    }
    _sourceDirs
  }

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    var elementsWriter: ElementsWriter       = _
    var relationsWriter: RelationshipsWriter = _
    var extensionWriter: ExtensionWriter     = _
    var traverser: SCUnitTraverser           = _
    var files: Set[String]                   = Set.empty

    var basePaths: Set[String] = Set.empty

    // TODO THis is a complete hack
    def workOutCommonSourcePath(files: Set[String]): String = {
      if (files.isEmpty)
        throw new IllegalStateException("No files")
      else if (files.size > 1) {
        throw new IllegalStateException(s"Multiple Source roots unsupported: $files")
      } else
        files.head
    }

    override def run(): Unit = {
      if (debug)
        global.reporter.echo("Before Analysis Phase")

      val outputPathBase: java.nio.file.Path =
        global.settings.outputDirs.getSingleOutput match {
          case Some(so) => so.file.toPath.toAbsolutePath
          case None     => Paths.get(global.settings.d.value)
        }

      val outputPath = outputPathBase.resolve("META-INF/ScalaClean/")
      outputPath.toFile.mkdirs()

      val elementsFile = new File(outputPath.toFile, "scalaclean-elements.csv")
      if (debug)
        println(s"Writing elements file  to $elementsFile")
      elementsWriter = new ElementsWriter(elementsFile)

      val relationsFile = new File(outputPath.toFile, "scalaclean-relationships.csv")
      if (debug)
        println(s"Writing relationships file to to $relationsFile")
      relationsWriter = new RelationshipsWriter(relationsFile)

      val extensionFile = new File(outputPath.toFile, "scalaclean-extensions.csv")
      if (debug)
        println(s"Writing extensions file to to $extensionFile")
      extensionWriter = new ExtensionWriter(extensionFile)

      traverser = new SCUnitTraverser(elementsWriter, relationsWriter, extensionWriter, debug)
      elementsWriter.logger = traverser
      relationsWriter.logger = traverser
      extensionWriter.logger = traverser

      super.run()

      val props = new Properties
      import PropertyNames._
      props.put(prop_sourceOsPathSeparator, java.io.File.pathSeparator)
      props.put(prop_sourceOsDirSeparator, java.io.File.separator)
      props.put(prop_classpath, global.settings.classpath.value)
      props.put(prop_outputDir, outputPathBase.toString)
      props.put(prop_elementsFile, elementsFile.toString)
      props.put(prop_relationshipsFile, relationsFile.toString)
      props.put(prop_extensionsFile, extensionFile.toString)
      if (debug)
        println("SourceDirs = " + sourceDirs)
      if (sourceDirs.nonEmpty) {
        props.put(prop_srcRoots, sourceDirs.mkString(File.pathSeparator))
      } else {
        val srcPath = workOutCommonSourcePath(basePaths)
        props.put(prop_srcRoots, srcPath)
      }

      options.foreach(p => props.put(s"$prefix_option.$p", ""))

      //      assert(sourceDirs.nonEmpty)

      val currentRelativePath = Paths.get("")
      val srcBuildBase        = currentRelativePath.toAbsolutePath.toString
      props.put(prop_srcBuildBase, srcBuildBase)
      props.put(prop_srcFiles, files.mkString(File.pathSeparator))

      val propsFile = outputPath.resolve(s"ScalaClean.properties")
      if (debug)
        println("Writing props file " + propsFile)
      props.store(Files.newBufferedWriter(propsFile), "")
      elementsWriter.finish()
      relationsWriter.finish()
      extensionWriter.finish()
      if (debug) {
        global.reporter.echo("After Analysis Phase")
        global.reporter.echo(s"  Wrote elements to $elementsFile")
        global.reporter.echo(s"  Wrote relationships to $relationsFile")

      }
    }

    override def apply(unit: global.CompilationUnit): Unit = {

      val srcPath = unit.source.file.toString

      files += srcPath

      val sourceFile = mungeUnitPath(unit.source.file.toString)
      basePaths += srcPath.substring(0, srcPath.indexOf(sourceFile))
      if (debug)
        global.reporter.echo(s"Executing for unit: $sourceFile")
      traverser.traverseSource(unit)
      elementsWriter.endUnit()
      relationsWriter.endUnit()
      extensionWriter.endUnit()
    }

  }

  trait ScopeTracking extends ScopeLogging {
    self: SCUnitTraverser =>

    var scopeStack: List[ModelSymbol] = Nil

    val debug: Boolean
    val logTransScope: Boolean

    var depth = 0

    private def indentString: String = "  " * depth

    def scopeLog(msg: => String): Unit = {
      if (debug)
        println(s"$indentString  $msg")
    }

    private var traversal = 0

    def enterScope[T, M <: ModelSymbol](mSymbol: M)(fn: M => T): Unit = {
      val outer = scopeStack.headOption
      if (outer.isEmpty) traversal = 0
      traversal += 1
      mSymbol.traversal = traversal
      outer.foreach(_.addChild(mSymbol))
      if (debug)
        println(s"$indentString${mSymbol.debugName}")
      scopeStack = mSymbol :: scopeStack
      depth += 1
      scopeLog(s"-symbol: ${mSymbol.legacyCsvIDString}")
      val oldVisited = newVisited()
      outer.foreach(o => mSymbol.addWithin(o))
      extensions.foreach { e =>
        val data = e.extendedData(mSymbol, mSymbol.tree.asInstanceOf[e.g.Tree], scopeStack.tail)
        mSymbol.addExtensionData(data)
      }
      traverseType(mSymbol.tree.tpe.asInstanceOf[global.Type])

      fn(mSymbol)
      scopeStack = scopeStack.tail
      depth -= 1
      resetVisited(oldVisited)
      if (debug)
        println(s"$indentString/${mSymbol.debugName}")
    }

    def outerScope: ModelSymbol = scopeStack.tail.head

    def currentScope: ModelSymbol = scopeStack.head

    def currentGlobalScope: ModelSymbol = scopeStack.find(_.isGlobal).get

    def hasCurrentGlobalScope: Boolean = scopeStack.nonEmpty

    def enterTransScope[T](name: String)(fn: => T): Unit = {

      if (debug && logTransScope) {
        println(s"$indentString$name")
        depth += 1
        fn
        depth -= 1
      } else
        fn
    }

  }

  class SCUnitTraverser(
      val elementsWriter: ElementsWriter,
      val relationsWriter: RelationshipsWriter,
      val extensionWriter: ExtensionWriter,
      val debug: Boolean
  ) extends global.Traverser
      with ScopeTracking {

    import global._

    lazy val g: global.type = global

    def recordExtendsClass(parentSym: HasModelCommon, childSym: ModelSymbol, direct: Boolean): Unit = {
      childSym.addExtends(parentSym, direct)
    }

    val logTransScope = true

    def traverseSource(unit: CompilationUnit): Unit = {
      val sourceFile    = unit.source.file.file.toPath
      val sourceFileStr = sourceFile.toString
      val sourceSymbol =
        ModelSource(unit.body, ModelCommon(true, ElementId(sourceFile), sourceFileStr, -1, -1, -1, "<NA>"))
      enterScope(sourceSymbol)(_ => traverse(unit.body))

      if (debug) {
        println("--- BEFORE -------------")
        sourceSymbol.printStructure()
        println("----------------")
      }
      sourceSymbol.flatten()
      if (debug) {
        println("--- AFTER -------------")
        sourceSymbol.printStructure()
        println("----------------")
      }
      sourceSymbol.outputStructure(elementsWriter, relationsWriter, extensionWriter)
      if (debug) {
        println("----------------")
      }
    }

    val initialVisited: Set[global.Symbol] = HashSet[global.Symbol](global.NoSymbol)

    def resetVisited(visited: Set[global.Symbol]) {
      visitedTypes = visited
    }

    def newVisited(): Set[global.Symbol] = {
      val existing = visitedTypes
      visitedTypes = initialVisited
      existing
    }

    newVisited()

    var visitedTypes: Set[global.Symbol] = HashSet.empty[global.Symbol]

    def traverseType(tpe: global.Type): Unit = {
      if (tpe ne null)
        traverseImpl(tpe)

      def add(symbol: global.Symbol): Boolean = {
        if (symbol eq global.NoSymbol) false
        else {
          val added = visitedTypes + symbol
          if (added eq visitedTypes) false
          else {
            visitedTypes = added
            currentScope.addRefers(asMSymbol(symbol), symbol.isSynthetic)
            true
          }
        }
      }

      def traverseImpl(tpe: global.Type) {
        tpe.foreach { tpePart =>
          val widened = tpePart.dealiasWiden
          val added1  = add(widened.termSymbol)
          val added2  = add(widened.typeSymbol)
          if (added1 || added2) widened match {
            case ref: global.TypeRefApi =>
              ref.args.foreach(traverseImpl)
            case _ =>
          }
        }
      }
    }

    def isObjectOrAny(sym: Symbol): Boolean = {
      sym == definitions.AnyClass || sym == definitions.ObjectClass
    }

    def recordOverrides(model: ClassLike): Unit = {
      val classSymbol = model.tree.symbol.asInstanceOf[global.Symbol]

      val directSymbols = classSymbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

      classSymbol.ancestors.foreach { ancestorSymbol =>
        val ancestorMSymbol = asMSymbol(ancestorSymbol)
        val direct          = directSymbols.contains(ancestorMSymbol)
        recordExtendsClass(ancestorMSymbol, model, direct = direct)
      }
      val cursor                                                                = new overridingPairs.Cursor(classSymbol)
      val seenJunctionsBetweenParents: mutable.Map[Symbol, mutable.Set[Symbol]] = new mutable.HashMap

      while (cursor.hasNext) {
        val entry = cursor.currentPair
        if (entry.low.owner == classSymbol) {
          model.remainingChildOverrides.getOrElseUpdate(entry.low, new mutable.HashSet[Global#Symbol]) += entry.high
        } else if (!isObjectOrAny(entry.low.owner) || !isObjectOrAny(entry.high.owner)) {

          val dummyMethodSym = entry.low.cloneSymbol(classSymbol)
          dummyMethodSym.setPos(classSymbol.pos.focusStart)
          dummyMethodSym.setFlag(Flags.SYNTHETIC)

          val targetSet = seenJunctionsBetweenParents.getOrElseUpdate(dummyMethodSym, new mutable.HashSet[Symbol])
          targetSet += entry.low
          targetSet += entry.high
        }
        cursor.next()
      }

      seenJunctionsBetweenParents.foreach { case (dummyMethodSym, targets) =>
        enterScope(
          ModelPlainMethod(
            DefDef(dummyMethodSym, new Modifiers(dummyMethodSym.flags, newTermName(""), Nil), global.EmptyTree),
            asMSymbol(dummyMethodSym),
            isTyped = false,
            isAbstract = false
          )
        ) { _ =>
          //if not recorded above, then maybe this should be a synthetic override
          targets.foreach(entry => currentScope.addOverride(asMSymbol(entry), direct = true))
        }
      }

    }

    def postProcess(model: ClassLike): Unit = {
      model.postProcess()
      if (model.remainingChildOverrides.nonEmpty) {
        scopeLog(s"add additional overrides in class but not in tree")
        model.remainingChildOverrides.foreach { case (l, parents) =>
          val local = l.asInstanceOf[global.Symbol]
          enterScope(
            ModelPlainMethod(
              DefDef(local, new Modifiers(local.flags, newTermName(""), Nil), global.EmptyTree),
              asMSymbol(local),
              isTyped = false,
              isAbstract = false
            )
          ) { _ =>
            //if not recorded above, then maybe this should be a synthetic override
            parents.foreach { e =>
              val entry = e.asInstanceOf[global.Symbol]
              currentScope.addOverride(asMSymbol(entry), direct = true)
            }
          }

        }
      }
    }

    override def traverse(tree: Tree): Unit = {

      tree match {
        case packageDef: PackageDef =>
          // ignore package symbol for now
          enterTransScope("PackageDef") {
            traverseType(tree.tpe)
            super.traverse(packageDef)
          }
        case treeSelect: Select =>
          enterTransScope("Select") {
            scopeLog("-symbol: " + asMSymbol(treeSelect.symbol).legacyCsvIDString)
            // avoids an issue with packages which we ScalaClean doesn't currently understand
            if (hasCurrentGlobalScope) {
              currentScope.addRefers(asMSymbol(treeSelect.symbol), isSynthetic = false)
            }
            traverseType(tree.tpe)
            super.traverse(treeSelect)
          }
        case template: Template =>
          enterTransScope("Template") {
            traverseType(tree.tpe)
            super.traverse(template)
          }
        case typeTree: TypeTree =>
          enterTransScope("TypeTree") {
            traverseType(tree.tpe)
            super.traverse(typeTree)
          }
        case blockTree: Block =>
          enterTransScope("Block") {
            traverseType(tree.tpe)
            super.traverse(blockTree)
          }
        case superTree: Super =>
          enterTransScope("Super") {
            traverseType(tree.tpe)
            super.traverse(superTree)
          }
        case EmptyTree =>
        // do nothing
        case _: This =>
          scopeLog("This")
        // do nothing
        case literalTree: Literal =>
          enterTransScope("Literal") {
            literalTree.attachments.get[analyzer.OriginalTreeAttachment].foreach(x => traverse(x.original))
            super.traverse(literalTree)
          }
        case identTree: Ident =>
          //          identTree.name
          enterTransScope("Ident " + identTree.symbol) {
            traverseType(tree.tpe)
            super.traverse(identTree)
            currentScope.addRefers(asMSymbol(identTree.symbol), identTree.symbol.isSynthetic)
          }

        //          scopeLog("Ident " + identTree.name)
        case _: Import =>
          //TODO need to track imports so that we can remove them
          scopeLog("Import")
        case objectDef: ModuleDef =>
          val symbol  = objectDef.symbol
          val mSymbol = asMSymbol(symbol)
          enterScope(ModelObject(objectDef, mSymbol)) { obj =>
            if (!symbol.owner.isPackageClass) {
              val parentMSym = asMSymbol(symbol.outerClass)
              if (parentMSym.common != outerScope.common) {
                scopeLog("WARNING outerClass symbol != scope")
                scopeLog("  parent (symbol.outerClass: " + parentMSym)
                scopeLog("  outerScope:                " + outerScope)
              }
            }
            recordOverrides(obj)
            super.traverse(tree)
            postProcess(obj)
          }

        case apply: Apply =>
          traverseType(tree.tpe)
          val target      = asMSymbol(apply.symbol)
          val isSynthetic = apply.symbol.isSynthetic
          currentScope.addRefers(target, isSynthetic)
          super.traverse(tree)

        // *********************************************************************************************************
        case classDef: ClassDef =>
          val symbol  = classDef.symbol
          val isTrait = symbol.isTrait
          val mSymbol = asMSymbol(symbol)

          val cls =
            if (isTrait) ModelTrait(classDef, mSymbol)
            else ModelClass(classDef, mSymbol, symbol.isAbstractClass)
          enterScope(cls) { cls =>
            recordOverrides(cls)
            super.traverse(tree)
            postProcess(cls)
          }

        // *********************************************************************************************************
        //cope with compound field declarations e.g. val (a,b,c,_ ) = ....
        case valDef: ValDef if valDef.mods.isArtifact && valDef.mods.isSynthetic =>
          val symbol = valDef.symbol
          val fields = ModelFields(valDef, asMSymbol(symbol), symbol.isLazy)
          enterScope(fields) { fields =>
            symbol.updateAttachment(fields)
            //we don't bother traversing the types of the symbol as they will be traversed on the actual fields
            super.traverse(tree)
          }
        //        case defDef: DefDef if defDef.mods.isArtifact && defDef.mods.isSynthetic && defDef.symbol.isAccessor =>
        ////          defDef.
        //          scopeLog(s"skip synthetic accessor ${defDef.name}")

        // *********************************************************************************************************
        case valDef: ValDef =>
          val symbol = valDef.symbol
          val isVar  = symbol.isVar
          val fields: Option[ModelFields] = valDef.rhs match {
            case Select(qualifier, name) =>
              Option(qualifier.symbol).flatMap(_.attachments.get[ModelFields])
            case _ => None
          }

          val field = currentScope match {
            case cls: ClassLike =>
              val getter  = symbol.getterIn(symbol.owner)
              val setter  = if (isVar) symbol.setterIn(symbol.owner) else NoSymbol
              val mSymbol = asMSymbolForceField(symbol)
              val field = if (isVar) {
                //                assert(setter != NoSymbol, s"no setter $mSymbol at ${valDef.pos.line}:${valDef.pos.column}")
                //                assert(getter != NoSymbol, s"no getter $mSymbol at ${valDef.pos.line}:${valDef.pos.column}")
                ModelVar(valDef, mSymbol, symbol.isDeferred, symbol.isParameter, fields)
              } else {
                //cant do this yet
                //there's no getter for the synthetic val in val (x,y) = ...
                //but we need other synthetic vals, and we need to better process this case
                //also for StaticAnnotation
                //assert(getter != NoSymbol, s"no getter $mSymbol at ${valDef.pos.line}:${valDef.pos.column}")

                ModelVal(valDef, mSymbol, symbol.isDeferred, valDef.symbol.isLazy, symbol.isParameter, fields)
              }
              cls.addPostProcess(() => {
                val getterSym = if (getter == NoSymbol) null else asMSymbol(getter)
                val setterSym = if (setter == NoSymbol) null else asMSymbol(setter)
                var added     = false
                if (getter != NoSymbol && !cls.children.contains(getterSym)) {
                  scopeLog(s"add getter for field $field as is wasn't added directly $getter")
                  added = true
                  enterScope(
                    ModelGetterMethod(
                      DefDef(getter, new Modifiers(getter.flags, newTermName(""), Nil), global.EmptyTree),
                      getterSym,
                      false,
                      false
                    )
                  ) { method =>
                    method.addGetterFor(field.common)
                    method.addedAccessor = true
                    addMethodOverrides(method, getter)
                  }
                }
                if (isVar && setter != NoSymbol && !cls.children.contains(setterSym)) {
                  scopeLog(s"add setter for field $field as is wasn't added directly $setter")
                  added = true
                  enterScope(
                    ModelSetterMethod(
                      DefDef(setter, new Modifiers(setter.flags, newTermName(""), Nil), global.EmptyTree),
                      asMSymbol(setter),
                      isTyped = false,
                      isAbstract = false
                    )
                  ) { method =>
                    method.addSetterFor(field.common)
                    method.addedAccessor = true
                    addMethodOverrides(method, setter)
                  }
                }
                //when children could not find the field - e.g. a trait var
                cls.children.foreach {
                  case (c, getter: ModelGetterMethod) if c == getterSym =>
                    if (!getter.addedAccessor) {
                      getter.addGetterFor(field.common)
                      getter.addedAccessor = true
                    }
                  case (c, setter: ModelSetterMethod) if c == setterSym =>
                    if (!setter.addedAccessor) {
                      setter.addSetterFor(field.common)
                      setter.addedAccessor = true
                    }
                  case _ =>
                }

                if (added)
                  scopeLog(s"end accessors for field $field")
              })
              field
            case _ =>
              val mSymbol = asMSymbol(symbol)
              if (isVar) ModelVar(valDef, mSymbol, symbol.isDeferred, symbol.isParameter, fields)
              else ModelVal(valDef, mSymbol, symbol.isDeferred, valDef.symbol.isLazy, symbol.isParameter, fields)
          }

          enterScope(field) { field =>
            field.addRefers(asMSymbol(valDef.tpt.symbol), symbol.isSynthetic)
            fields.foreach(_.addField(field))
            //TODO why this restriction - if anything why not the inverse?
            if (symbol.isLocalToBlock) {

              def typeRels(typeTarget: Type): Unit = {
                val typeSymbol = typeTarget.typeSymbol
                currentScope.addRefers(asMSymbol(typeSymbol), typeSymbol.isSynthetic)
                typeTarget.typeArgs.foreach(tpe => typeRels(tpe))
              }

              typeRels(valDef.tpt.tpe)
            }

            super.traverse(tree)
          }

        // *********************************************************************************************************
        case defdef: DefDef =>
          // TODO This feels wrong - this is def declType Defined field
          val declTypeDefined = defdef.isTyped
          val symbol          = defdef.symbol
          val mSymbol         = asMSymbol(symbol)

          def traverseMethod(method: ModelMethod): Unit = {
            if (symbol.isConstructor)
              //FIXME - only for the declared extends/with
              symbol.enclClass.info.parents.foreach {
                case t if t.typeSymbol.isTrait =>
                  val init = t.decl(newTermName("$init$"))
                  if (init != g.NoSymbol) {
                    currentScope.addRefers(asMSymbol(init), isSynthetic = false)
                  }
                case _ =>
              }

            if (symbol.owner.isClass) {
              addMethodOverrides(method, symbol)
            }

            super.traverse(tree)

          }

          currentScope match {
            case o: ModelObject if symbol.isConstructor =>
              // we consider the constructor of the object to be part of the object
              // this simplified the model navigation
              super.traverse(tree)
            case cls: ModelClass if symbol.isConstructor =>
              // constructor params can also be fields, so we alias them
              // (ScalaClean is only interested the val, but we need to see usage)
              // so that the traversal sees the usage of the param as a usage of the field
              val symbolAliases: List[Symbol] = {
                for (params <- symbol.paramss;
                     param <- params;
                     field = symbol.enclClass.tpe.decl(param.name);
                //its a value, and the positions overlap
                     if field.isValue && field.pos.start <= param.pos.`end` && field.pos.`end` >= param.pos.start;
                     model <- cls.findChildBySymbol(field)) yield {
                  addAlias(param, model)
                  param
                }
              }
              enterScope(ModelPlainMethod(defdef, mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
              }
              symbolAliases foreach removeAlias
            case _ if symbol.isAccessor && symbol.isGetter =>
              enterScope(ModelGetterMethod(defdef, mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
                if (symbol.accessedOrSelf != symbol) {
                  method.addGetterFor(asMSymbol(symbol.accessedOrSelf))
                  method.addedAccessor = true
                }
              }
            case _ if symbol.isAccessor && symbol.isSetter =>
              enterScope(ModelSetterMethod(defdef, mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
                if (symbol.accessedOrSelf != symbol) {
                  method.addSetterFor(asMSymbol(symbol.accessedOrSelf))
                  method.addedAccessor = true
                }
              }

            case _ if symbol.isSynthetic =>
              enterScope(ModelPlainMethod(defdef, mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
              }
            case _ =>
              enterScope(ModelPlainMethod(defdef, mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
              }
          }

        case New(tpt) =>
          scopeLog(s"--  NEW tree ${tpt.symbol} - ${asMSymbol(tpt.symbol)}")
          traverseType(tree.tpe)
          super.traverse(tree)
        case unknown =>
          scopeLog("--  unhandled tree" + tree.getClass)
          traverseType(tree.tpe)
          super.traverse(tree)
      }
    }

    def addMethodOverrides(method: ModelMethod, symbol: Symbol): Unit = method.withinRels.head match {
      case clsDirectParent: ClassLike =>
        val classSym = symbol.owner

        val directSymbols: Set[global.Symbol] = clsDirectParent.removeChildOveride(symbol) match {
          case None => Set()
          case Some(syms) =>
            syms.asInstanceOf[mutable.Set[global.Symbol]].toSet
        }

        directSymbols.foreach(s => method.addOverride(asMSymbol(s), direct = true))

        //        val directParentSymbols: Set[ModelCommon] = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet
        //
        //        scopeLog(s"DirectParentSymbols = $directParentSymbols")
        //
        //        val directParentSymbols2 = symbol.outerClass
        //
        //        scopeLog(s"DirectParentSymbols2 = $classSym")
        //        scopeLog(s"DirectParentSymbols2 = $directParentSymbols2")
        //
        //        val directClassParentSymbols = classSym.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet
        //
        //        scopeLog(s"DirectParentSymbols3 = $directClassParentSymbols")
        //
        //        classSym.ancestors foreach { ancestorSymbol =>
        //          val ancestorMSymbol = asMSymbol(ancestorSymbol)
        //          val direct = directSymbols.contains(ancestorSymbol)
        //          val overridden = symbol.overriddenSymbol(ancestorSymbol)
        //          if (overridden != NoSymbol) {
        //            scopeLog(s"    AAAAA ${overridden} $direct")
        //          }
        //        }

        symbol.overrides.foreach { overridden =>
          //          val overriddenOwnerMSym = asMSymbol(overridden.owner)
          if (!directSymbols.contains(overridden))
            method.addOverride(asMSymbol(overridden), direct = false)
        }

      //if it not a top level method then it cant override anything
      case _ =>
    }

  }

}
