package scalaclean.privatiser

import scalaclean.model.{ModelHelper, ScalaCleanModel}
import scalaclean.util.DefaultTreeVisitor
import scalafix.v1._


class ScalaCleanPrivatiserAnalysis extends SemanticRule("ScalaCleanPrivatiserAnalysis")  {
  val model = new ScalaCleanModel()

  override def beforeStart(): Unit = {
    println("Analysis BEFORE START")
  }

  override def afterComplete(): Unit = {
    model.finishedParsing()
    ModelHelper.model = Some(model)
    println(s"Analysis AFTER COMPLETE ${model.size}")
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    model.analyse(doc)
    Patch.empty
  }

}
