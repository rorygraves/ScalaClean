package scalaclean

import scalaclean.model.{ModelHelper, ScalaCleanModel}
import scalafix.v1._

class Analysis extends SemanticRule("Analysis")  {
  val model = new ScalaCleanModel()

  override def beforeStart(): Unit = {
    println("Analysis BEFORE START")
  }

  override def afterComplete(): Unit = {
    model.finishedParsing()
    ModelHelper.model = Some(model)
    println(s"Analysis AFTER COMPLETE size = ${model.size}")
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    model.analyse(doc)
    Patch.empty
  }
}
