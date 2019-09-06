package org.scalaclean.analysis

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with SemanticdbOps {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"
  override val runsAfter: List[String] = List("typer")

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    override def run(): Unit = {
      global.reporter.echo("Before Analysis Phase")
      super.run()
      global.reporter.echo("After Analysis Phase")

    }

    override def apply(unit: global.CompilationUnit): Unit = {
      global.reporter.echo(s"Executing for unit: ${unit.source}")
      import global._
      class SCTraverser extends Traverser  {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case defdef: DefDef =>
              println(defdef.symbol.toSemantic)
              println(defdef)
            case _ =>
          }

          super.traverse(tree)
        }
      }
      (new SCTraverser).traverse(unit.body)
    }
  }
}
