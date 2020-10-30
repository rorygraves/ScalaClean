package org.scalaclean.analysis

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.text.{DateFormat, SimpleDateFormat}
import java.util
import java.util.{Date, Properties, UUID}

import org.scalaclean.analysis.plugin.ExtensionPlugin
import scala.collection.mutable
import scala.reflect.internal.Flags
import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}
import scala.util.hashing.MurmurHash3

class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with ModelSymbolBuilder {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"

  override val runsAfter: List[String] = List("typer")
  // a bit ugly, but the options are read after the component is create - so it is updated by the plugin
  var copySources                = false
  var debug                      = false
  var extensions                 = Set.empty[ExtensionPlugin]
  var options: List[String]      = Nil
  var outputParent: Option[Path] = None
  var sourcesRoot: List[Path]    = Nil

  def sourceDirs: List[Path] = sourcesRoot

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    var sourceCopier:    SourceCopier        = _
    var elementsWriter:  ElementsWriter      = _
    var relationsWriter: RelationshipsWriter = _
    var extensionWriter: ExtensionWriter     = _
    var traverser: SCUnitTraverser           = _
    var files: Set[Path]                     = Set.empty

    override def run(): Unit = {
      if (debug)
        global.reporter.echo("Before Analysis Phase")

      val outputPath: java.nio.file.Path = outputParent match {
        case Some(root) =>
          val prefix = new SimpleDateFormat("yyyy_MM_dd-").format(new Date())
          val dir    = root.resolve(s"$prefix${UUID.randomUUID()}")
          Files.createDirectory(dir)
          dir
        case None =>
          val base = global.settings.outputDirs.getSingleOutput match {
            case Some(so) => so.file.toPath.toAbsolutePath
            case None     => Paths.get(global.settings.d.value)
          }
          base.resolve("META-INF/ScalaClean/")
      }
      Files.createDirectories(outputPath)

      if (copySources)
        sourceCopier = new JarSourceCopier(outputPath.resolve("sources.jar"))
      else sourceCopier = NoSourceCopier
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

      traverser =
        new SCUnitTraverser(elementsWriter, relationsWriter, extensionWriter, sourceCopier, debug, global.settings.encoding.value)
      elementsWriter.logger = traverser
      relationsWriter.logger = traverser
      extensionWriter.logger = traverser

      try {
        super.run()

        val props = new Properties
        import PropertyNames._
        props.put(prop_sourceOsPathSeparator, java.io.File.pathSeparator)
        props.put(prop_sourceOsDirSeparator, java.io.File.separator)
        props.put(prop_classpath, global.settings.classpath.value)
        props.put(prop_outputDir, outputPath.toString)
        props.put(prop_elementsFile, elementsFile.toString)
        props.put(prop_relationshipsFile, relationsFile.toString)
        props.put(prop_extensionsFile, extensionFile.toString)
        if (debug) {
          println("SourceDirs = " + sourceDirs)
          println("Output parent = " + outputPath)
        }
        if (sourceDirs.nonEmpty) {
          props.put(prop_srcRoots, sourceDirs.mkString(File.pathSeparator))
        } else {
          props.put(prop_srcRoots, "/")
        }

        val currentRelativePath = Paths.get("")
        val srcBuildBase = currentRelativePath.toAbsolutePath.toString
        props.put(prop_srcBuildBase, srcBuildBase)
        props.put(prop_srcFiles, files.mkString(File.pathSeparator))

        val propsFile = outputPath.resolve(s"ScalaClean.properties")
        if (debug)
          println("Writing props file " + propsFile)
        props.store(Files.newBufferedWriter(propsFile), "")
        if (debug) {
          global.reporter.echo("After Analysis Phase")
          global.reporter.echo(s"  Wrote elements to $elementsFile")
          global.reporter.echo(s"  Wrote relationships to $relationsFile")

        }
      } finally {
        sourceCopier.close()
        elementsWriter.finish()
        relationsWriter.finish()
        extensionWriter.finish()
      }
    }

    override def apply(unit: global.CompilationUnit): Unit = {

      val sourceFile = sanePath(unit.source.file)

      files += sourceFile

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
      scopeLog(s"-symbol: ${mSymbol.common.elementId}")
      val oldVisited = newVisited()
      outer.foreach(o => mSymbol.setWithin(o))
      extensions.foreach { e =>
        val data = e.extendedData(
          mSymbol,
          mSymbol.tree.asInstanceOf[Option[e.g.Tree]],
          mSymbol.symbol.asInstanceOf[e.g.Symbol],
          scopeStack.tail)
        mSymbol.addExtensionData(data)
      }
      traverseType(mSymbol.symbol.tpe.asInstanceOf[global.Type])

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
      val sourceCopier: SourceCopier,
      val debug: Boolean,
      val fileEncoding: String
  ) extends global.Traverser
      with ScopeTracking {

    import global._

    lazy val g: global.type = global

    def recordExtendsClass(parentSym: HasModelCommon, childSym: ModelSymbol, direct: Boolean): Unit = {
      childSym.addExtends(parentSym, direct)
    }

    val logTransScope = true

    def traverseSource(unit: CompilationUnit): Unit = {
      val sourceFile = sanePath(unit.source.file)
      val content = new String(unit.source.content)
      val sourceSymbol = {
        ModelSource(
          unit.body,
          ModelCommon(isGlobal = true, elementIds(sourceFile), sourceFile, -1, -1, -1, "<NA>"),
          fileEncoding,
          content.length,
          content.hashCode,
          MurmurHash3.stringHash(content)
        )
      }
      sourceCopier.copySource(sourceSymbol, content)
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

    type VisitedType = util.Map[AnyRef, java.lang.Boolean]
    val initialVisited:VisitedType = new util.IdentityHashMap[AnyRef, java.lang.Boolean]()
    initialVisited.put(NoSymbol, java.lang.Boolean.TRUE)
    initialVisited.put(NoType, java.lang.Boolean.TRUE)

    def resetVisited(resetTypes: VisitedType) {
      visited = resetTypes
    }

    def newVisited(): VisitedType= {
      val existing = visited
      visited = new util.IdentityHashMap[AnyRef, java.lang.Boolean](initialVisited)
      existing
    }
    def isFirstVisit(to: AnyRef): Boolean = {
      visited.put(to, java.lang.Boolean.TRUE) eq null
    }

    newVisited()

    var visited: VisitedType = initialVisited

    def add(symbol: global.Symbol): Boolean = {
      if (symbol eq global.NoSymbol) false
      else {
        val added = isFirstVisit(symbol)
        if (added) {
          currentScope.addRefers(asMSymbol(symbol), symbol.isSynthetic)
          traverseAnnotationInfos(symbol.annotations)
        }
        added
      }
    }
    def traverseType(tpe: global.Type): Unit = {
      if (tpe ne null)
        traverseImpl(tpe)


      def traverseImpl(tpe: global.Type) {
        if (isFirstVisit(tpe)) {
          traverseAnnotationInfos(tpe.annotations)
          add(tpe.termSymbol)
          add(tpe.typeSymbol)
          tpe.foreach { tpePart =>
            val widened = tpePart.dealiasWiden
            val added1 = add(widened.termSymbol)
            val added2 = add(widened.typeSymbol)
            if (added1 || added2) widened match {
              case ref: global.TypeRefApi =>
                ref.args.foreach(traverseImpl)
              case _ =>
            }
          }
        }
      }
    }

    def isObjectOrAny(sym: Symbol): Boolean = {
      sym == definitions.AnyClass || sym == definitions.ObjectClass
    }
    private def recordOverrides(classLike: ClassLike, classSymbolXX: global.Symbol) = {
      val classSymbol: global.ClassSymbol = classLike match {
        case obj: ModelObject => obj.symbol.moduleClass.asClass.asInstanceOf[global.ClassSymbol]
        case _ => classLike.symbol.asClass.asInstanceOf[global.ClassSymbol]
      }
      val cursor = new overridingPairs.Cursor(classSymbol)
      val direct = mutable.Map[Symbol, mutable.Set[Symbol]]()
      val indirect = mutable.Map[Symbol, mutable.Set[Symbol]]()
      while (cursor.hasNext) {
        val entry = cursor.currentPair

        def addTo(data: mutable.Map[Symbol, mutable.Set[Symbol]]): Unit =
          data.getOrElseUpdate(entry.low, mutable.Set[Symbol]()) += entry.high

        if (entry.low.isMethod) {
          //we dont consider types ATM

          if (entry.low.owner == classSymbol)
            addTo(direct)
          else
          //can we trim the ones that we know we dont participate in, less data churn?
          //if (!entry.low.isEffectivelyFinalOrNotOverridden)
          //
          //we should probably limit the parents to be ones that we cant recover, so parents that are compiled in this run
          //if (currentRun.compiles(entry.low))
          //maybe trim the Object/Any relationships as well
            addTo(indirect)
        }
        cursor.next()
      }
      def processParents(localMethod: ModelMethod, parentSym: Symbol, synthetic: Boolean): Unit = {
        for (indirectParents <- indirect.remove(parentSym);
             indirectParent <- indirectParents) {
          localMethod.addOverride(asMSymbol(indirectParent), direct = false, synthetic = synthetic)
          processParents(localMethod, indirectParent, synthetic)
        }
      }
      for ((baseSym, parents) <- direct;
           directMethod = classLike.getChildBySymbol[ModelMethod](baseSym);
           parent <- parents) {
        directMethod.addOverride(asMSymbol(parent), direct = true, synthetic = false)
        processParents(directMethod, parent, false)
      }
      while(indirect.nonEmpty) {
        val (baseSym, parents) = indirect.head
        indirect.remove(baseSym)

        val sym = baseSym.cloneSymbol(classSymbol)
        sym.pos = classSymbol.pos.focus
        val syntheticMethod = ModelSyntheticMethod(sym, asMSymbol(sym))
        enterScope(syntheticMethod) { method =>
          method.addOverride(asMSymbol(baseSym), direct = true, synthetic = true)
          parents foreach { parent =>
            method.addOverride(asMSymbol(parent), direct = true, synthetic = true)
            processParents(method, parent, true)
          }
        }
      }
    }

    def recordExtends(classLike: ClassLike, classSymbol: global.Symbol): Unit = {

      val directSymbols = classSymbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

      classSymbol.ancestors.foreach { ancestorSymbol =>
        val ancestorMSymbol = asMSymbol(ancestorSymbol)
        val direct = directSymbols.contains(ancestorMSymbol)
        recordExtendsClass(ancestorMSymbol, classLike, direct = direct)
      }

    }
    def handleClassLike(classLike: ClassLike): Unit = {
      val classSymbol = classLike.symbol.asInstanceOf[global.Symbol]
      recordExtends(classLike, classSymbol)
      postProcess(classLike)
      // we post-process before we add the overrides, as a trait val is just a ValDef, so a ModelField
      // and post-process will add a getter if one isnt defined
      // and recordOverrides needs that getter/setter to wire up
      recordOverrides(classLike, classSymbol)

    }

    def postProcess(model: ClassLike): Unit = {
      model.postProcess()
      model match {
        case cls: ModelClass =>
          //cope with constructor vals
          val ctorSym    = model.symbol.asClass.primaryConstructor
          val ctorParams = ctorSym.paramss.flatten.groupBy(_.nameString)

          model.children.values.foreach {
            case field: ModelField =>
              //we have to trim because the compiler has trailing spaces
              ctorParams.get(field.symbol.nameString.trim) match {
                case Some(ctorParam) => field.addConstructorParam(asMSymbol(ctorParam.head.asInstanceOf[global.Symbol]))
                case None            =>
              }
            case _ =>
          }
        case _ =>
      }
    }

    def traverseClassFileAnnotation(arg: ClassfileAnnotArg): Unit = {
      if (isFirstVisit(arg)) arg match {
        case LiteralAnnotArg(const) => traverseType(const.tpe)
        case ArrayAnnotArg(array) => array foreach traverseClassFileAnnotation
        case NestedAnnotArg(info) => traverseAnnotationInfo(info)
        case UnmappableAnnotArg =>
        case ScalaSigBytes(_) =>
      }
    }

    def traverseAnnotationInfos(anns: Seq[AnnotationInfo]): Unit = anns foreach traverseAnnotationInfo

    def traverseAnnotationInfo(ann: AnnotationInfo): Unit = {
      if (isFirstVisit(ann)) {
        traverseAnnotationInfos(ann.metaAnnotations)
        traverseType(ann.tpe)
        traverse(ann.original)
        ann.assocs foreach { case (_, arg) => traverseClassFileAnnotation(arg) }
        traverseParams(ann.scalaArgs)
      }
    }
    def traverseInnards(tree: Tree): Unit = {
      traverseType(tree.tpe)
      if (tree.symbol ne null) {
        val sym = tree.symbol
        add(sym)
        if (sym.tpe ne tree.tpe) traverseType(sym.tpe)
      }
      super.traverse(tree)
    }

    def isInterestingValDef(valDef: ValDef): Boolean = {
      if (valDef.symbol.hasFlag(Flags.PARAM)) {
        //we keep parameters of methods that we keep
        currentScope match {
          case method: ModelMethod =>
            val res = method.symbol == valDef.symbol.owner
            res
          case _ =>
            false
        }
      }
      else
        isChildOfClassLike(valDef)
    }
    def isChildOfClassLike(tree: Tree): Boolean = {
      val res = currentScope match {
        case _: ModelObject =>
          tree.symbol.owner == currentScope.symbol.moduleClass
        case _: ClassLike =>
          tree.symbol.owner == currentScope.symbol
        case _: ModelField => false
        case _: ModelMethod => false
        case _ => ???
      }
      res
    }

    override def traverse(tree: Tree): Unit = {

      tree match {
        case packageDef: PackageDef =>
          // ignore package symbol for now
          enterTransScope("PackageDef") {
            traverseInnards(tree)
          }
        case treeSelect: Select =>
          enterTransScope("Select") {
            scopeLog("-symbol: " + asMSymbol(treeSelect.symbol).common.elementId)
            // avoids an issue with packages which we ScalaClean doesn't currently understand
            if (hasCurrentGlobalScope) {
              currentScope.addRefers(asMSymbol(treeSelect.symbol), isSynthetic = false)
            }
            traverseInnards(tree)
          }
        case template: Template =>
          enterTransScope("Template") {
            traverseInnards(tree)
          }
        case typeTree: TypeTree =>
          enterTransScope("TypeTree") {
            traverseInnards(tree)
          }
        case blockTree: Block =>
          enterTransScope("Block") {
            traverseInnards(tree)
          }
        case superTree: Super =>
          enterTransScope("Super") {
            traverseInnards(tree)
          }
        case EmptyTree =>
        // do nothing
        case _: This =>
          scopeLog("This")
        // do nothing
        case literalTree: Literal =>
          enterTransScope("Literal") {
            literalTree.attachments.get[analyzer.OriginalTreeAttachment].foreach(x => traverse(x.original))
            traverseInnards(tree)
            literalTree.value.value match {
              case x: Type =>
                traverseType(x)
              case x: Symbol =>
                add(x)
              case _ =>
            }
          }
        case identTree: Ident =>
          //          identTree.name
          enterTransScope("Ident " + identTree.symbol) {
            traverseInnards(tree)
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
            traverseInnards(tree)
            handleClassLike(obj)
          }

        case apply: Apply =>
          val target      = asMSymbol(apply.symbol)
          val isSynthetic = apply.symbol.isSynthetic
          currentScope.addRefers(target, isSynthetic)
          traverseInnards(tree)

        // *********************************************************************************************************
        case classDef: ClassDef =>
          val symbol  = classDef.symbol
          val isTrait = symbol.isTrait
          val mSymbol = asMSymbol(symbol)

          val cls =
            if (isTrait) ModelTrait(classDef, mSymbol)
            else ModelClass(classDef, mSymbol, symbol.isAbstractClass)
          enterScope(cls) { cls =>
            traverseInnards(tree)
            handleClassLike(cls)
            // cope with a self type
            // NOTE - we cant use classDef.symbol.hasSelfType because we have to cope with having a thisSym
            // which is the same type - e.g. class Foo { x => } . Not sure why but this only fails on some classes only though
            if (classDef.symbol.thisSym ne classDef.symbol) {
              val thisSym = classDef.symbol.thisSym
              relationsWriter.recordSelfTypeField(cls, cls.findChildBySymbol[ModelField](thisSym).get)
            }
          }

        // *********************************************************************************************************
        //cope with compound field declarations e.g. val (a,b,c,_ ) = ....
        case valDef: ValDef if valDef.mods.isArtifact && valDef.mods.isSynthetic && isChildOfClassLike(valDef) =>
          val symbol = valDef.symbol
          val fields = ModelFields(valDef, asMSymbol(symbol), symbol.isLazy)
          enterScope(fields) { fields =>
            symbol.updateAttachment(fields)
            //we don't bother traversing the types of the symbol as they will be traversed on the actual fields
          }
        //        case defDef: DefDef if defDef.mods.isArtifact && defDef.mods.isSynthetic && defDef.symbol.isAccessor =>
        ////          defDef.
        //          scopeLog(s"skip synthetic accessor ${defDef.name}")

        // *********************************************************************************************************
        case valDef: ValDef if isInterestingValDef(valDef) =>
          //its only a var due to for https://github.com/scala/bug/issues/12213 -- see below
          var symbol = valDef.symbol
          val isVar  = symbol.isVar
          val fields: Option[ModelFields] = valDef.rhs match {
            case Select(qualifier, name) =>
              Option(qualifier.symbol).flatMap(_.attachments.get[ModelFields])
            case _ => None
          }
          //workaround for https://github.com/scala/bug/issues/12213
          if (isVar && valDef.rhs.isEmpty) {
            val scanner = newUnitScanner(new CompilationUnit(valDef.pos.source))
            scanner.ch = ' '
            scanner.lastOffset = valDef.pos.`end`
            scanner.offset = scanner.lastOffset + 1
            scanner.charOffset = scanner.offset

            scanner.nextToken
            if (scanner.token == scala.tools.nsc.ast.parser.Tokens.EQUALS) {
              scanner.nextToken
              if (scanner.token == scala.tools.nsc.ast.parser.Tokens.USCORE) {
                val realEnd = scanner.offset + 1
                val oldPos  = symbol.pos
                symbol = symbol.cloneSymbol(symbol.owner)
                symbol.pos = new RangePosition(oldPos.source, oldPos.start, oldPos.point, realEnd)
              }
            }
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
                      isTyped = false,
                      isAbstract = false
                    )
                  ) { method =>
                    method.addGetterFor(field.common)
                    method.addedAccessor = true
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
            traverseInnards(tree)
          }

        // *********************************************************************************************************
        case defdef: DefDef if isChildOfClassLike(defdef)=>
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


            traverseInnards(tree)
            for (
              params <- defdef.vparamss;
              param  <- params
            ) {
              if (param.symbol.hasFlag(Flags.DEFAULTPARAM)) {
                method.children
                  .collectFirst {
                    case (common, field: ModelField) if field.symbol == param.symbol => field
                  }
                  .foreach { field =>
                    field.addDefaultGetter(
                      asMSymbol(global.analyzer.defaultGetter(param.symbol, global.analyzer.NoContext))
                    )
                  }
              }
            }

          }

          currentScope match {
            case o: ModelObject if symbol.isConstructor =>
              // we consider the constructor of the object to be part of the object
              // this simplified the model navigation
              super.traverse(tree)
            case cls: ModelClass if symbol.isConstructor =>
//              // constructor params can also be fields, so we alias them
//              // ScalaClean is interested the val, but we need to see usage, so retain both symbols, but keep a
//              // relationship between them, and the rules need to check for that relationship - e.g. a val on a ctor and
//              // the class val share the same source position, so any rewrites affect both
//              val symbolAliases: List[Symbol] = {
//                for (params <- symbol.paramss;
//                     param <- params;
//                     field = symbol.enclClass.tpe.decl(param.name)
//                     //its a value, and the positions overlap
//                     if field.isValue && field.pos.start <= param.pos.`end` && field.pos.`end` >= param.pos.start;
//                     modelField <- cls.findChildBySymbol[ModelField](field)) yield {
//                  modelField.addCtorSymbol(param)
//                  param
//                }
//              }
              enterScope(ModelPlainMethod(defdef, mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
              }
//              symbolAliases foreach removeAlias
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
          traverseInnards(tree)
        case unknown =>
          scopeLog("--  unhandled tree" + tree.getClass)
          traverseInnards(tree)
      }
    }


  }

}
