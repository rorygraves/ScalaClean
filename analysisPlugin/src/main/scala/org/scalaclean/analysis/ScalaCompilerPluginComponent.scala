package org.scalaclean.analysis

import java.io.File

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.io.AbsolutePath
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with SemanticdbOps with ModelSymbolBuilder {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"
  override val runsAfter: List[String] = List("typer")

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    var elementsWriter: ElementsWriter = _

    var relationsWriter: RelationshipsWriter = _

    override def run(): Unit = {
      global.reporter.echo("Before Analysis Phase")

      val outputPath = AbsolutePath(
        global.settings.outputDirs.getSingleOutput
          .flatMap(so => Option(so.file))
          .map(_.getAbsolutePath)
          .getOrElse(global.settings.d.value))

      val elementsFile = new File(outputPath.toFile, "scalaclean-elements.new.csv")
      println(s"Writing elements file  to ${elementsFile}")
      elementsWriter = new ElementsWriter(elementsFile)

      val relationsFile = new File(outputPath.toFile, "scalaclean-relationships.new.csv")
      println(s"Writing relationships file to to ${relationsFile}")
      relationsWriter = new RelationshipsWriter(relationsFile, global)


      super.run()
      elementsWriter.finish()
      relationsWriter.finish()
      global.reporter.echo("After Analysis Phase")

    }

    object PlatformPathIO {
      def workingDirectoryString: String =
        sys.props("user.dir")
    }

//    // TODO this is a total hack - need to discover the source root of the compilation unit and remove
//    def mungeUnitPath(input: String): String = {
//      val idx = input.indexOf("src/main/scala")
//      if (idx != -1)
//        input.substring(idx + "src/main/scala".length + 1)
//      else input
//    }
//
    override def apply(unit: global.CompilationUnit): Unit = {

      val sourceFile = mungeUnitPath(unit.source.file.toString)

      global.reporter.echo(s"Executing for unit: ${sourceFile}")


      import global._
      class SCTraverser extends Traverser {
        override def traverse(tree: Tree): Unit = {
          println("--" + tree.getClass + "  " + Option(tree.tpe).map(_.nameAndArgsString))
          tree match {
            case objectDef: ModuleDef =>
              val symbol = objectDef.symbol
              val mSymbol = asMSymbol(symbol)
              val sSymbol = symbol.toSemantic
              val isGlobal = symbol.isSemanticdbGlobal
              elementsWriter.objectDef(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)
              println("object: " + sSymbol)
              symbol.ancestors foreach { parentSymbol =>
                relationsWriter.extendsCls(asMSymbol(parentSymbol), mSymbol)
                println(s"  parent: ${parentSymbol.toSemantic}")
              }
            case apply: Apply =>
              println("-----------------------------------------------")
              newTreePrinter().print(apply)

              println(asMSymbol(apply.symbol).csvString)
              println(asMSymbol(apply.symbol.owner).csvString)
              println("-----------------------------------------------")
            case classDef: ClassDef =>
              val symbol = classDef.symbol
              val mSymbol = asMSymbol(symbol)
              val isTrait = symbol.isTrait
              val sSymbol = symbol.toSemantic
              val isGlobal = symbol.isSemanticdbGlobal
              if (!symbol.isSynthetic) {
                if(isTrait)
                  elementsWriter.traitDef(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)
                else
                  elementsWriter.classDef(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)
              }

              println("class: " + sSymbol)

              symbol.ancestors foreach { parentSymbol =>
                relationsWriter.extendsCls(asMSymbol(parentSymbol), mSymbol)
                println(s"  parent: ${parentSymbol.toSemantic}")
              }

            case valDef: ValDef =>

              val symbol = valDef.symbol
              val owner = symbol.owner
              if (owner.isClass || owner.isModuleOrModuleClass || owner.isPackageObjectOrClass) {
                val mSymbol = asMSymbol(symbol)
                if (!symbol.isSynthetic)
                  if (symbol.isVar) {
                    elementsWriter.varDef(mSymbol)
                    println("var: " + mSymbol.csvString)
                  } else {
                    elementsWriter.valDef(mSymbol)
                    println("val: " + mSymbol.csvString)
                  }

                val parentMSym = asMSymbol(symbol.outerClass)
                relationsWriter.within(parentMSym, mSymbol)
              }


            case defdef: DefDef =>

              // TODO This feels wrong - this is def declType Defined field
              val declTypeDefined = defdef.isTyped
              val symbol = defdef.symbol
              val mSymbol = asMSymbol(symbol)
              if (!symbol.isSynthetic && !symbol.isAccessor) {
                elementsWriter.method(mSymbol,symbol.nameString,declTypeDefined)
                println("def: " + mSymbol.csvString)
              }

              val parentMSym = asMSymbol(symbol.outerClass)
              relationsWriter.within(parentMSym, mSymbol)

            case unknown =>

          }

          super.traverse(tree)
        }
      }
      (new SCTraverser).traverse(unit.body)
    }
  }
}
