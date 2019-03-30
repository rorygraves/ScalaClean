package scalaclean

import scalaclean.model.{ModelHelper, ScalaCleanModel}
import scalafix.v1._

class Analysis extends SemanticRule("Analysis")  {
  val model =  ScalaCleanModel.createParseModel

  override def beforeStart(): Unit = {
    println("Analysis BEFORE START")
  }

  override def afterComplete(): Unit = {
    model.finishedParsing()
    val projectModel = model.asProjectModel
    ModelHelper.model = Some(projectModel)
    println(s"Analysis AFTER COMPLETE size = ${projectModel.size}")
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    model.analyse(doc)
    Patch.empty
  }
}
