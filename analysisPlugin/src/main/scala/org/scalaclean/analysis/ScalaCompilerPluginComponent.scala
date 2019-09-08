package org.scalaclean.analysis

import java.io.File

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.io.AbsolutePath
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with SemanticdbOps {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"
  override val runsAfter: List[String] = List("typer")

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    var elementsWriter: ElementsWriter = _
    override def run(): Unit = {
      global.reporter.echo("Before Analysis Phase")

      val outputPath = AbsolutePath(
        global.settings.outputDirs.getSingleOutput
          .flatMap(so => Option(so.file))
          .map(_.getAbsolutePath)
          .getOrElse(global.settings.d.value))

      val outputFile = new File(outputPath.toFile ,"scalaclean-data.csv")
      println(s"Writing analysis results to ${outputFile}")
      elementsWriter = new ElementsWriter(outputFile)
      super.run()
      elementsWriter.finish()
      global.reporter.echo("After Analysis Phase")

    }

    object PlatformPathIO {
      def workingDirectoryString: String =
        sys.props("user.dir")
    }

    // TODO this is a total hack - need to discover the source root of the compilaion unit and remove
    def mungeUnitPath(input: String): String = {
      val idx = input.indexOf("src/main/scala")
      if(idx != -1)
        input.substring(idx + "src/main/scala".length + 1)
      else input
    }
    override def apply(unit: global.CompilationUnit): Unit = {

      val sourceFile = mungeUnitPath(unit.source.file.toString)

      global.reporter.echo(s"Executing for unit: ${sourceFile}")


      import global._
      class SCTraverser extends Traverser  {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case classDef: ClassDef =>
              val symbol = classDef.symbol
              val sSymbol = symbol.toSemantic
              val isGlobal = symbol.isSemanticdbGlobal
              if(!symbol.isSynthetic)
                elementsWriter.classDef(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)
              println("class: "+ symbol.toSemantic)

            case defdef: DefDef =>
              val symbol = defdef.symbol
              val sSymbol = symbol.toSemantic
              val isGlobal = symbol.isSemanticdbGlobal
              if(!symbol.isSynthetic)
                elementsWriter.method(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)
              println("def: " + defdef.symbol.toSemantic)
            case _ =>
          }

          super.traverse(tree)
        }
      }
      (new SCTraverser).traverse(unit.body)
    }
  }
}
