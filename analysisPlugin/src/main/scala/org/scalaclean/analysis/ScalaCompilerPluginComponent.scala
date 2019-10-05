package org.scalaclean.analysis

import java.io.File
import java.nio.file.Files
import java.util.Properties

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.io.AbsolutePath
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with SemanticdbOps with ModelSymbolBuilder {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"
  override val runsAfter: List[String] = List("typer")

  // a bit ugly, but the options are read after the component is create - so it is updated by the plugin
  var debug = false

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    var elementsWriter: ElementsWriter = _


    var relationsWriter: RelationshipsWriter = _
    var files: Set[String] = Set.empty

    var basePaths: Set[String] = Set.empty

    // TODO THis is a complete hack
    def workOutCommonSourcePath(files: Set[String]) : String = {
      if(files.isEmpty)
        throw new IllegalStateException("No files")
      else if(files.size > 1) {
        throw new IllegalStateException(s"Multiple Source roots unsupported: $files")
      } else
        files.head
    }

    override def run(): Unit = {
      if(debug)
        global.reporter.echo("Before Analysis Phase")

      val outputPathBase: AbsolutePath = AbsolutePath(
        global.settings.outputDirs.getSingleOutput
          .flatMap(so => Option(so.file))
          .map(v => v.getAbsolutePath)
          .getOrElse(global.settings.d.value))

      val outputPath = outputPathBase.resolve("META-INF/ScalaClean/")
      outputPath.toFile.mkdirs()
      val elementsFile = new File(outputPath.toFile, "scalaclean-elements.csv")

      if(debug)
        println(s"Writing elements file  to ${elementsFile}")
      elementsWriter = new ElementsWriter(elementsFile)


      val relationsFile = new File(outputPath.toFile, "scalaclean-relationships.csv")

      if(debug)
        println(s"Writing relationships file to to ${relationsFile}")
      relationsWriter = new RelationshipsWriter(relationsFile, global)


      super.run()

      val props = new Properties
      props.put("classpath", global.settings.classpath.value)
      props.put("outputDir", outputPathBase.toString())
      props.put("elementsFile", elementsFile.toString)
      props.put("relationshipsFile", relationsFile.toString)
      val srcPath = workOutCommonSourcePath(basePaths)
      props.put("src", srcPath)

      props.put("srcFiles", files.mkString(File.pathSeparator))

      val propsFile = outputPath.resolve(s"ScalaClean.properties").toNIO
      if(debug)
        println("Writing props file " + propsFile)
      props.store(Files.newBufferedWriter(propsFile),"")
      elementsWriter.finish()
      relationsWriter.finish()
      if(debug) {
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
      if(debug)
        global.reporter.echo(s"Executing for unit: ${sourceFile}")
      (new SCUnitTraverser(sourceFile, elementsWriter, relationsWriter, debug)).traverse(unit.body)
    }
  }

  trait ScopeTracking {

    var scopeStack: List[ModelSymbol] = Nil

    val debug: Boolean
    val logTransScope: Boolean

    var depth = 0
    private def indentString: String = "  " * depth
    def scopeLog(msg: => String): Unit = {
      if(debug)
        println(s"${indentString}  $msg")
    }


    def enterScope[T](name: String, mSymbol: ModelSymbol)(fn: => T): Unit = {
      if(debug)
        println(s"${indentString}$name")
      scopeStack = mSymbol :: scopeStack
      depth +=1
      scopeLog(s"-symbol: ${mSymbol.csvString}")
      fn
      scopeStack = scopeStack.tail
      depth -=1
    }


    def outerScope: ModelSymbol = scopeStack.tail.head
    def currentScope: ModelSymbol = scopeStack.head
    def currentGlobalScope: ModelSymbol = scopeStack.find(_.isGlobal).get
    def hasCurrentGlobalScope: Boolean = scopeStack.nonEmpty

    def enterTransScope[T](name: String)(fn: => T): Unit = {

      if(debug && logTransScope) {
        println(s"${indentString}$name")
        depth +=1
        fn
        depth -=1
      } else
        fn
    }

  }


  class SCUnitTraverser(sourceFile: String, elementsWriter: ElementsWriter, relationsWriter: RelationshipsWriter, val debug: Boolean) extends global.Traverser with ScopeTracking {

    import global._
    lazy val g: global.type = global

    def recordExtendsClass(parentSym: ModelSymbol, childSym: ModelSymbol, direct: Boolean): Unit = {
      relationsWriter.extendsCls(parentSym, childSym, direct = direct)
      scopeLog(s"-extendsCls: ${parentSym.csvString}  direct=$direct")
    }

    val logTransScope = true

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
            if(hasCurrentGlobalScope) {
              scopeLog("--refers:" + asMSymbol(treeSelect.symbol).csvString)
              relationsWriter.refers(currentGlobalScope, asMSymbol(treeSelect.symbol), false)
            }
            super.traverse(treeSelect)
          }
        case template: Template =>
          enterTransScope("Template")(super.traverse(template))
        case typeTree: TypeTree =>
          enterTransScope("TypeTree") {
//            println(typeTree)
//            println(typeTree.symbol)
//
            super.traverse(typeTree) }
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

//          scopeLog("Ident " + identTree.name)
        case importTree: Import =>
          scopeLog("Import")
        case objectDef: ModuleDef =>
          val symbol = objectDef.symbol
          val mSymbol = asMSymbol(symbol)
          enterScope("ObjectDef", mSymbol) {
            val sSymbol = symbol.toSemantic
            val isGlobal = symbol.isSemanticdbGlobal
            elementsWriter.objectDef(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)

            val directSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

            if (!symbol.owner.isPackageClass) {
              val parentMSym = asMSymbol(symbol.outerClass)
              if(parentMSym != outerScope) {
                scopeLog("WARNING outerClass symbol != scope")
                scopeLog("  parent (symbol.outerClass: " + parentMSym)
                scopeLog("  outerScope:                " + outerScope)
              }
              relationsWriter.within(outerScope, mSymbol)
            }

            symbol.ancestors foreach { parentSymbol =>
              val parentMSymbol = asMSymbol(parentSymbol)
              val direct = directSymbols.contains(parentMSymbol)
              relationsWriter.extendsCls(parentMSymbol, mSymbol, direct)
            }

            super.traverse(tree)
          }

        case apply: Apply =>
          val target = asMSymbol(apply.symbol)
          val isSynthetic = target.isSynthetic
          if(!isSynthetic && !scopeStack.head.isSynthetic)
            relationsWriter.refers(currentGlobalScope, target, isSynthetic)
          super.traverse(tree)

        case classDef: ClassDef =>
          val symbol = classDef.symbol
          val isTrait = symbol.isTrait
          val mSymbol = asMSymbol(symbol)

          enterScope("ClassDef", mSymbol) {

            if (!symbol.isSynthetic) {
              if (isTrait)
                elementsWriter.traitDef(mSymbol)
              else
                elementsWriter.classDef(mSymbol)
            }

            val directSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

            symbol.ancestors foreach { ancestorSymbol =>
              val ancestorMSymbol = asMSymbol(ancestorSymbol)
              val direct = directSymbols.contains(ancestorMSymbol)
              recordExtendsClass(ancestorMSymbol, mSymbol, direct = direct)

            }
            super.traverse(tree)
          }

        // *********************************************************************************************************
        case valDef: ValDef =>

          val symbol = valDef.symbol
          val mSymbol = asMSymbol(symbol)
          enterScope("ValDef-" + (if(symbol.isVar) "var" else "val"), mSymbol) {
            relationsWriter.refers(currentScope, asMSymbol(valDef.tpt.symbol), symbol.isSynthetic)
            if (symbol.isLocalToBlock) {

              def typeRels(container: ModelSymbol, typeTarget: Type): Unit = {
                val typeSymbol = typeTarget.typeSymbol
                relationsWriter.refers(currentScope, asMSymbol(typeSymbol), typeSymbol.isSynthetic)
                typeTarget.typeArgs foreach { tpe =>
                  typeRels(container, tpe)
                }
              }

              typeRels(currentScope, valDef.tpt.tpe)
            }
            if (symbol.isVar) {
              elementsWriter.varDef(mSymbol)
              scopeLog("var: " + mSymbol.csvString)
            } else {
              elementsWriter.valDef(mSymbol)
              scopeLog("val: " + mSymbol.csvString)
              val parentMSym = asMSymbol(symbol.outerClass)
              relationsWriter.within(parentMSym, mSymbol)
              relationsWriter.refers(parentMSym, mSymbol, symbol.isSynthetic)
            }

            super.traverse(tree)
          }
        case defdef: DefDef =>

          // TODO This feels wrong - this is def declType Defined field
          val declTypeDefined = defdef.isTyped
          val symbol = defdef.symbol
          val mSymbol = asMSymbol(symbol)

          enterScope("DefDef " + symbol.nameString, mSymbol) {
            if (!symbol.isSynthetic && !symbol.isAccessor) {
              elementsWriter.method(mSymbol, symbol.nameString, declTypeDefined)

              val parentMSym = asMSymbol(symbol.outerClass)
              if(parentMSym != outerScope)
                println("xXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
              relationsWriter.within(parentMSym, mSymbol)

              val directParentSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

              symbol.overrides.foreach { overridden =>
                val overriddenOwnerMSym = asMSymbol(overridden.owner)
                val direct = directParentSymbols.contains(overriddenOwnerMSym)

                relationsWriter.overrides(mSymbol, asMSymbol(overridden), direct)
              }

              super.traverse(tree)
            }
          }

        case unknown =>
          scopeLog("--  unhandled tree" + tree.getClass)
          super.traverse(tree)
      }
    }
  }
}
