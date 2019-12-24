package org.scalaclean.analysis

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerBeforeTyperPluginComponent(
  val global: Global) extends PluginComponent  {
  override val phaseName: String = "scalaclean-compiler-early-plugin-phase"

  import global._

  override val runsBefore: List[String] = List("typer")
  override val runsAfter: List[String] = List("parser")

  override val runsRightAfter: Option[String] = Some("parser")

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    override def run(): Unit = {
      global.reporter.echo("Early Phase")

      super.run()
    }


    override def apply(unit: global.CompilationUnit): Unit = {
      traverser.traverseSource(unit)
    }


    object traverser extends global.Traverser {
      def traverseSource(unit: CompilationUnit): Unit = {
        traverse(unit.body)
      }


      override def traverse(tree: Tree): Unit = {
        tree match {
          case d: MemberDefApi =>
            println(s"XXXX 1 $d")
            println(s"XXXX 2 ${d.mods}")
            d.mods.positions foreach {case (f,p) => println(s"XXXX 3 ${FlagHelper.hexAndString(f)} @ $p")}
          case _ =>

        }
        super.traverse(tree)

      }
    }

  }
}
