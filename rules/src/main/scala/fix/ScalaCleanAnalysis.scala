package fix

import fix.util.TreeVisitor
import scalaclean.model.{ModelHelper, ScalaCleanModel}
import scalafix.v1._


class ScalaCleanAnalysis extends SemanticRule("ScalaCleanAnalysis")  {
  val model = new ScalaCleanModel()

  override def beforeStart(): Unit = {
    println("Analysis BEFORE START")
  }

  override def afterComplete(): Unit = {
    model.finishedParsing()
    ModelHelper.model = Some(model)
    println("Analysis AFTER COMPLETE")
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    val tv = new TreeVisitor
    tv.visitDocument(doc.tree)
    println("-------------------------")
    model.analyse(doc)
    Patch.empty
  }

}
