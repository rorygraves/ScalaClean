package scalaclean.test

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.RuleRun
import scalafix.v1.SyntacticDocument

import scala.collection.JavaConverters._
import scala.meta.AbsolutePath

/**
 * A rule use to test that plugin marks are set correctly
 */
class ShowColour(theModel: ProjectModel, pluginParams: List[String]) extends TestCommon("ShowColour", theModel) {
  val cmdLine = new ScalaCleanCommandLine {}
  cmdLine._rulePlugins = pluginParams.asJava
  dummyRule.markInitial()
  dummyRule.beforeRule()
  dummyRule.afterRule()

  override def visitInSource(modelElement: ModelElement): String = {
    if (modelElement.mark.changesAreBanned) {
      s"Banned - ${modelElement.mark.changeInhibitors}"
    } else modelElement.mark.specific match {
      case Some(x) =>
        s"Specific - ${x}"
      case None => ""
    }
  }

  object dummyRule extends RuleRun[ScalaCleanCommandLine] {
    override val options: ScalaCleanCommandLine = cmdLine
    override val model: ProjectModel = theModel
    override type SpecificColour = ASpecificColour

    override def runRule(): Unit = ???

    override def fix(sModel: SourceModel, syntacticDocument: () => SyntacticDocument): List[SCPatch] = Nil
  }
  class ASpecificColour extends SomeSpecificColour {
    override type RealType = ASpecificColour

    override def merge(other: ASpecificColour): ASpecificColour = ???
  }
}
