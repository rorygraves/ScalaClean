package scalaclean.deadcode

import scalaclean.model.{ModelHelper, ScalaCleanModel}
import scalaclean.util.DefaultTreeVisitor
import scalafix.v1._


class ScalaCleanDeadCodeAnalysis extends SemanticRule("ScalaCleanDeadCodeAnalysis")  {
  val model = new ScalaCleanModel()

  override def beforeStart(): Unit = {
    println("Analysis (dead code) BEFORE START")
  }

  override def afterComplete(): Unit = {
    model.finishedParsing()
    ModelHelper.model = Some(model)
    println(s"Analysis (dead code) AFTER COMPLETE size = ${model.size}")
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
//    model.analyse(doc)
    Patch.empty
  }

}
