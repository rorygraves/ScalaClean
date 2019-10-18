package org.scalaclean.analysis

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.Properties

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.io.AbsolutePath
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with SemanticdbOps with ModelSymbolBuilder {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"
  override val runsAfter: List[String] = List("semanticdb-typer")

  // a bit ugly, but the options are read after the component is create - so it is updated by the plugin
  var debug = false
  var sourceDirs: List[String] = List.empty

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    var elementsWriter: ElementsWriter = _
    var relationsWriter: RelationshipsWriter = _
    var traverser: SCUnitTraverser = _
    var files: Set[String] = Set.empty

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

      val outputPathBase: AbsolutePath = AbsolutePath(
        global.settings.outputDirs.getSingleOutput
          .flatMap(so => Option(so.file))
          .map(v => v.getAbsolutePath)
          .getOrElse(global.settings.d.value))

      val outputPath = outputPathBase.resolve("META-INF/ScalaClean/")
      outputPath.toFile.mkdirs()
      val elementsFile = new File(outputPath.toFile, "scalaclean-elements.csv")


      if (debug)
        println(s"Writing elements file  to ${elementsFile}")
      elementsWriter = new ElementsWriter(elementsFile)


      val relationsFile = new File(outputPath.toFile, "scalaclean-relationships.csv")

      if (debug)
        println(s"Writing relationships file to to ${relationsFile}")
      relationsWriter = new RelationshipsWriter(relationsFile, global)

      traverser = new SCUnitTraverser(elementsWriter, relationsWriter, debug)
      elementsWriter.logger = traverser
      relationsWriter.logger = traverser

      super.run()

      val props = new Properties
      props.put("classpath", global.settings.classpath.value)
      props.put("outputDir", outputPathBase.toString())
      props.put("elementsFile", elementsFile.toString)
      props.put("relationshipsFile", relationsFile.toString)
      println("SourceDirs = " + sourceDirs)
      if (sourceDirs.nonEmpty) {
        props.put("srcRoots", sourceDirs.mkString(File.pathSeparator))
        props.put("src", sourceDirs.head)
      } else {
        val srcPath = workOutCommonSourcePath(basePaths)
        props.put("srcRoots", srcPath)
        props.put("src", srcPath)
      }

      //      assert(sourceDirs.nonEmpty)

      val currentRelativePath = Paths.get("")
      val srcBuildBase = currentRelativePath.toAbsolutePath.toString
      props.put("srcBuildBase", srcBuildBase)
      props.put("srcFiles", files.mkString(File.pathSeparator))

      val propsFile = outputPath.resolve(s"ScalaClean.properties").toNIO
      if (debug)
        println("Writing props file " + propsFile)
      props.store(Files.newBufferedWriter(propsFile), "")
      elementsWriter.finish()
      relationsWriter.finish()
      if (debug) {
        global.reporter.echo("After Analysis Phase")
        global.reporter.echo(s"  Wrote elements to $elementsFile")
        global.reporter.echo(s"  Wrote relationships to $relationsFile")

      }
    }

    object PlatformPathIO {
      def workingDirectoryString: String =
        sys.props("user.dir")
    }


    override def apply(unit: global.CompilationUnit): Unit = {

      val srcPath = unit.source.file.toString

      files += srcPath

      val sourceFile = mungeUnitPath(unit.source.file.toString)
      basePaths += srcPath.substring(0, srcPath.indexOf(sourceFile))
      if (debug)
        global.reporter.echo(s"Executing for unit: ${sourceFile}")
      traverser.traverseSource(unit)
      elementsWriter.endUnit()
      relationsWriter.endUnit()
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
        println(s"${indentString}  $msg")
    }


    def enterScope[T, M <: ModelSymbol](mSymbol: M)(fn: M => T): Unit = {
      val outer = scopeStack.headOption
      if (debug)
        println(s"${indentString}${mSymbol.debugName}")
      scopeStack = mSymbol :: scopeStack
      depth += 1
      scopeLog(s"-symbol: ${mSymbol.csvString}")
      outer.foreach { o =>
        relationsWriter.within(o, mSymbol)
      }
      elementsWriter.write(mSymbol)
      fn(mSymbol)
      scopeStack = scopeStack.tail
      depth -= 1
    }


    def outerScope: ModelSymbol = scopeStack.tail.head

    def currentScope: ModelSymbol = scopeStack.head

    def currentGlobalScope: ModelSymbol = scopeStack.find(_.isGlobal).get

    def hasCurrentGlobalScope: Boolean = scopeStack.nonEmpty

    def enterTransScope[T](name: String)(fn: => T): Unit = {

      if (debug && logTransScope) {
        println(s"${indentString}$name")
        depth += 1
        fn
        depth -= 1
      } else
        fn
    }

  }


  class SCUnitTraverser(val elementsWriter: ElementsWriter, val relationsWriter: RelationshipsWriter, val debug: Boolean) extends global.Traverser with ScopeTracking {

    import global._

    lazy val g: global.type = global

    def recordExtendsClass(parentSym: HasModelCommon, childSym: ModelSymbol, direct: Boolean): Unit = {
      relationsWriter.extendsCls(parentSym, childSym, direct = direct)
    }

    val logTransScope = true

    def traverseSource(unit: CompilationUnit): Unit = {
      val sourceFile = unit.source.file.canonicalPath
      enterScope(ModelSource(ModelCommon(true, s"source:$sourceFile", sourceFile, -1, -1, "<NA>"))) {
        s =>
          traverse(unit.body)
      }
    }

    override def traverse(tree: Tree): Unit = {

      tree match {
        case packageDef: PackageDef =>
          val symbol = packageDef.symbol
          val mSymbol = asMSymbol(symbol)

          // ignore package symbol for now
          enterTransScope("PackageDef") {
            super.traverse(packageDef)
          }
        case treeSelect: Select =>
          enterTransScope("Select") {
            scopeLog("-symbol: " + asMSymbol(treeSelect.symbol).csvString)
            // avoids an issue with packages which we ScalaClean doesn't currently understand
            if (hasCurrentGlobalScope) {
              relationsWriter.refers(currentGlobalScope, asMSymbol(treeSelect.symbol), false)
            }
            super.traverse(treeSelect)
          }
        case template: Template =>
          enterTransScope("Template")(super.traverse(template))
        case typeTree: TypeTree =>
          enterTransScope("TypeTree") {
            super.traverse(typeTree)
          }
        case blockTree: Block =>
          enterTransScope("Block")(super.traverse(blockTree))
        case superTree: Super =>
          enterTransScope("Super")(super.traverse(superTree))
        case EmptyTree =>
        // do nothing
        case thisTree: This =>
          scopeLog("This")
        // do nothing
        case literalTree: Literal =>
          scopeLog("Literal " + literalTree)
        case identTree: Ident =>
          //          identTree.name
          enterTransScope("Ident " + identTree.symbol)(super.traverse(identTree))
          relationsWriter.refers(currentGlobalScope, asMSymbol(identTree.symbol), identTree.symbol.isSynthetic)

        //          scopeLog("Ident " + identTree.name)
        case importTree: Import =>
          //TODO need to track imports so that we can remove them
          scopeLog("Import")
        case objectDef: ModuleDef =>
          val symbol = objectDef.symbol
          val mSymbol = asMSymbol(symbol)
          enterScope(ModelObject(mSymbol)) { obj =>

            val directSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

            if (!symbol.owner.isPackageClass) {
              val parentMSym = asMSymbol(symbol.outerClass)
              if (parentMSym.common != outerScope.common) {
                scopeLog("WARNING outerClass symbol != scope")
                scopeLog("  parent (symbol.outerClass: " + parentMSym)
                scopeLog("  outerScope:                " + outerScope)
              }
            }

            symbol.ancestors foreach { parentSymbol =>
              val parentMSymbol = asMSymbol(parentSymbol)
              val direct = directSymbols.contains(parentMSymbol)
              relationsWriter.extendsCls(parentMSymbol, obj, direct)
            }

            super.traverse(tree)
          }

        case apply: Apply =>
          val target = asMSymbol(apply.symbol)
          val isSynthetic = apply.symbol.isSynthetic
          relationsWriter.refers(currentGlobalScope, target, isSynthetic)
          super.traverse(tree)

        case classDef: ClassDef =>
          val symbol = classDef.symbol
          val isTrait = symbol.isTrait
          val mSymbol = asMSymbol(symbol)

          val cls =
            if (isTrait) ModelTrait(mSymbol)
            else ModelClass(mSymbol, symbol.isAbstractClass)
          enterScope(cls) { cls =>

            val directSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

            symbol.ancestors foreach { ancestorSymbol =>
              val ancestorMSymbol = asMSymbol(ancestorSymbol)
              val direct = directSymbols.contains(ancestorMSymbol)
              recordExtendsClass(ancestorMSymbol, cls, direct = direct)

            }
            super.traverse(tree)
          }

        // *********************************************************************************************************
        case valDef: ValDef =>

          val symbol = valDef.symbol
          val mSymbol = asMSymbol(symbol)
          val field =
            if (symbol.isVar) ModelVar(mSymbol, symbol.isDeferred)
            else ModelVal(mSymbol, symbol.isDeferred, valDef.symbol.isLazy)
          enterScope(field) { field =>
            relationsWriter.refers(field, asMSymbol(valDef.tpt.symbol), symbol.isSynthetic)
            if (symbol.isLocalToBlock) {

              def typeRels(typeTarget: Type): Unit = {
                val typeSymbol = typeTarget.typeSymbol
                relationsWriter.refers(currentScope, asMSymbol(typeSymbol), typeSymbol.isSynthetic)
                typeTarget.typeArgs foreach { tpe =>
                  typeRels(tpe)
                }
              }

              typeRels(valDef.tpt.tpe)
            }

            super.traverse(tree)
          }
        case defdef: DefDef =>

          // TODO This feels wrong - this is def declType Defined field
          val declTypeDefined = defdef.isTyped
          val symbol = defdef.symbol
          val mSymbol = asMSymbol(symbol)

          def traverseMethod(method: ModelMethod): Unit = {
            val directParentSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

            symbol.overrides.foreach { overridden =>
              val overriddenOwnerMSym = asMSymbol(overridden.owner)
              val direct = directParentSymbols.contains(overriddenOwnerMSym)

              relationsWriter.overrides(method, asMSymbol(overridden), direct)
            }

            super.traverse(tree)

          }

          currentScope match {
            case o: ModelObject if defdef.symbol.nameString == "<init>" =>
              // we consider the constructor of the object to be part of the object
              // this simplified the model navigation
              super.traverse(tree)
            case _ if symbol.isAccessor && symbol.isGetter =>
              val field = asMSymbol(symbol.accessedOrSelf)
              enterScope(ModelGetterMethod(mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
                relationsWriter.getterFor(mSymbol, field)
              }
            case _ if symbol.isAccessor && symbol.isSetter =>
              val field = asMSymbol(symbol.accessedOrSelf)
              enterScope(ModelSetterMethod(mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
                relationsWriter.setterFor(mSymbol, field)
              }

            case _ if symbol.isSynthetic =>
            // TODO should we super.traverse(tree) ??
            case _ =>

              enterScope(ModelPlainMethod(mSymbol, declTypeDefined, symbol.isDeferred)) { method =>
                traverseMethod(method)
              }
          }

        case New(x) =>
          scopeLog("--  NEW tree" + tree.getClass)
          super.traverse(tree)
        case unknown =>
          scopeLog("--  unhandled tree" + tree.getClass)
          super.traverse(tree)
      }
    }
  }

}
