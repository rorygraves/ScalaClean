package org.scalaclean.analysis

import java.io.File

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.io.AbsolutePath
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with SemanticdbOps with ModelSymbolBuilder {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"
  override val runsAfter: List[String] = List("typer") // was typer

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    val debug = false

    var elementsWriter: ElementsWriter = _

    var relationsWriter: RelationshipsWriter = _

    override def run(): Unit = {
      global.reporter.echo("Before Analysis Phase")

      val outputPathBase: AbsolutePath = AbsolutePath(
        global.settings.outputDirs.getSingleOutput
          .flatMap(so => Option(so.file))
          .map(v => v.getAbsolutePath)
          .getOrElse(global.settings.d.value))


      val outputPath = outputPathBase.resolve("META-INF/ScalaClean/")
      outputPath.toFile.mkdirs()
      val elementsFile = new File(outputPath.toFile, "scalaclean-elements.csv")
      println(s"Writing elements file  to ${elementsFile}")
      elementsWriter = new ElementsWriter(elementsFile)

      val relationsFile = new File(outputPath.toFile, "scalaclean-relationships.csv")
      println(s"Writing relationships file to to ${relationsFile}")
      relationsWriter = new RelationshipsWriter(relationsFile, global)


      super.run()
      elementsWriter.finish()
      relationsWriter.finish()
      global.reporter.echo("After Analysis Phase")
      global.reporter.echo(s"  Wrote elements to $elementsFile")
      global.reporter.echo(s"  Wrote relationships to $relationsFile")

    }

    object PlatformPathIO {
      def workingDirectoryString: String =
        sys.props("user.dir")
    }

    override def apply(unit: global.CompilationUnit): Unit = {

      val sourceFile = mungeUnitPath(unit.source.file.toString)

      global.reporter.echo(s"Executing for unit: ${sourceFile}")

      var scopeStack: List[ModelSymbol] = Nil

      def withinScope[T](mSymbol: ModelSymbol)(fn: => T): Unit = {
        if(debug)
          println("  " * scopeStack.size + "   Entering " + mSymbol)
        scopeStack = mSymbol :: scopeStack
        fn
        scopeStack = scopeStack.tail
        if(debug)
          println("  " * scopeStack.size +"   Exiting " + mSymbol)
      }

      def parentScope: ModelSymbol = scopeStack.head
      def parentGlobalScope: ModelSymbol = scopeStack.find(_.isGlobal).get

      import global._
      class SCTraverser extends Traverser {
        override def traverse(tree: Tree): Unit = {
//          if(debug)
            if(tree.symbol != NoSymbol && tree.symbol != null) {
              println("--" + tree.getClass +  "  --  " + tree.symbol.toSemantic + "  " + tree.symbol.pos + " " + Option(tree.tpe).map(_.nameAndArgsString))
            }

          tree match {
            case objectDef: ModuleDef =>
              val symbol = objectDef.symbol
              val mSymbol = asMSymbol(symbol)
              val sSymbol = symbol.toSemantic
              val isGlobal = symbol.isSemanticdbGlobal
              elementsWriter.objectDef(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)
              if(debug)
                println("object: " + sSymbol)

              val directSymbols =  symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

              if(!symbol.owner.isPackageClass) {
                val parentMSym = asMSymbol(symbol.outerClass)
                relationsWriter.within(parentMSym, mSymbol)
              }

              symbol.ancestors foreach { parentSymbol =>
                val parentMSymbol = asMSymbol(parentSymbol)
                val direct = directSymbols.contains(parentMSymbol)
                relationsWriter.extendsCls(asMSymbol(parentSymbol), mSymbol,direct)
                if(debug)
                  println(s"  parent: ${parentSymbol.toSemantic}  $direct")
              }

              withinScope(mSymbol) {
                super.traverse(tree)
              }
            case apply: Apply =>
              val target = asMSymbol(apply.symbol)
              val isSynthetic = target.isSynthetic
              if(!isSynthetic && !scopeStack.head.isSynthetic)
                relationsWriter.refers(parentGlobalScope, target, isSynthetic)
              super.traverse(tree)

            case classDef: ClassDef =>
              val symbol = classDef.symbol
              val isTrait = symbol.isTrait
              val mSymbol = asMSymbol(symbol)
              if (!symbol.isSynthetic) {
                if(isTrait)
                  elementsWriter.traitDef(mSymbol)
                else
                  elementsWriter.classDef(mSymbol)
              }

//              if(debug)
                println("-------------------------------------------------")
                println("class: " + mSymbol.csvString)

              val directSymbols =  symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

              symbol.ancestors foreach { parentSymbol =>
                val parentMSymbol = asMSymbol(parentSymbol)
                val direct = directSymbols.contains(parentMSymbol)
                relationsWriter.extendsCls(parentMSymbol, mSymbol,direct = direct)
                println(s"  parent: ${parentSymbol.toSemantic}  $direct")
              }
              withinScope(mSymbol) {
                super.traverse(tree)
              }

            // *********************************************************************************************************
            case valDef: ValDef =>

              val symbol = valDef.symbol
              println("\n\nVAL  DEF " + valDef.symbol)
              val mSymbol = asMSymbol(symbol)
              relationsWriter.refers(parentScope,asMSymbol(valDef.tpt.symbol),symbol.isSynthetic)
              if(symbol.isLocalToBlock) {
                println("------------")
                println("  " +   valDef.tpt)
                println(" tpt.XXXX " + valDef.tpt.symbol)
                println("  tpt.CLS " + valDef.tpt.getClass)
                println("  tpt.PARENTS " + valDef.tpt.symbol.parentSymbols)
                println("  tpt.TPE " + valDef.tpt.tpe)
                println("  tpt.TPE " + valDef.tpt.tpe.typeSymbol.nameString)
                valDef.tpt.tpe.foreach { x =>
                  println("  tpt.TPE_X " + x)
                }

                def typeRels(container: ModelSymbol, typeTarget: Type): Unit = {
                  val typeSymbol = typeTarget.typeSymbol
                  relationsWriter.refers(parentScope,asMSymbol(typeSymbol),typeSymbol.isSynthetic)
                  typeTarget.typeArgs foreach { tpe =>
                    typeRels(container, tpe)
//                    relationsWriter.refers(parentScope,asMSymbol(typeSymbol),typeSymbol.isSynthetic)
                  }
                }

                typeRels(parentScope, valDef.tpt.tpe)
              }
              val owner = symbol.owner
//              if (owner.isClass || owner.isModuleOrModuleClass || owner.isPackageObjectOrClass) {
                if (!symbol.isSynthetic) {
                  if (symbol.isVar) {
                    elementsWriter.varDef(mSymbol)
                    if(debug)
                      println("var: " + mSymbol.csvString)
                  } else {
                    elementsWriter.valDef(mSymbol)
                    if(debug)
                      println("val: " + mSymbol.csvString)
                  }
                  val parentMSym = asMSymbol(symbol.outerClass)
                  relationsWriter.within(parentMSym, mSymbol)
                  relationsWriter.refers(parentMSym, mSymbol, symbol.isSynthetic)
                }
  //            }

              withinScope(mSymbol) {
                super.traverse(tree)
              }

            case defdef: DefDef =>

              println("\n\nDEF DEF " + defdef.symbol)
              // TODO This feels wrong - this is def declType Defined field
              val declTypeDefined = defdef.isTyped
              val symbol = defdef.symbol
              val mSymbol = asMSymbol(symbol)
              if (!symbol.isSynthetic && !symbol.isAccessor) {
                elementsWriter.method(mSymbol,symbol.nameString,declTypeDefined)
                if(debug)
                  println("def: " + mSymbol.csvString)

                val parentMSym = asMSymbol(symbol.outerClass)
                relationsWriter.within(parentMSym, mSymbol)

                val directParentSymbols =  symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

                symbol.overrides.foreach { overridden =>
                  val overrriddenOwnerMSym =asMSymbol(overridden.owner)
                  val direct = directParentSymbols.contains(overrriddenOwnerMSym)

                  relationsWriter.overrides(mSymbol, asMSymbol(overridden), direct)
                }

                withinScope(mSymbol) {
                  super.traverse(tree)
                }
              }

            case unknown =>
              println("  -- " + tree)
              super.traverse(tree)

          }

        }
      }
      (new SCTraverser).traverse(unit.body)
    }
  }
}
