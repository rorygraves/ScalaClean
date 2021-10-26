package scalaclean.test

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model._
import scalaclean.rules.{RuleRun, SourceFile}
import scalaclean.util.SingleFileVisit

import scala.jdk.CollectionConverters._

/**
 * A rule use to test that plugin marks are set correctly
 */
class ShowColour(theModel: AllProjectsModel, pluginParams: List[String]) extends TestCommon("ShowColour", theModel) {
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
    override val model: AllProjectsModel = theModel
    override type SpecificColour = ASpecificColour

    override def runRule(): Unit = ???

    override def generateFixes(sourceFile: SourceFile): SingleFileVisit = ???
  }
  class ASpecificColour extends SomeSpecificColour {
    override type RealType = ASpecificColour

    override def merge(other: ASpecificColour): ASpecificColour = ???
  }
}
