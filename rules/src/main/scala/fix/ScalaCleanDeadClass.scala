package fix

import scalaclean.model.{ModelHelper, SCModel}
import scalafix.v1._

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanAnalysis
  */
class ScalaCleanDeadClass extends SemanticRule("ScalaCleanDeadClass")  {
  var model: SCModel = _


  override def beforeStart(): Unit = {
    println("Cleaner Rule BEFORE START")
    // TODO - where do we get the config path to load from - check other rules for examples

    // hack to load the model from the helper class
    this.model = ModelHelper.model.getOrElse(throw new IllegalStateException("No model to work from"))
    println("Structure ----------------")
    model.printStructure()
    println("Structure end ----------------")

  }

  override def afterComplete(): Unit = {
    println("Cleaner Rule AFTER COMPLETE")
  }

  def isClassDead(name: String): Boolean = {
    // TODO This should depend on the SCModel
    name == "fix.UnusedClass"
  }

  override def fix(implicit doc: SemanticDocument): Patch = {

    // TODO traverse the tree and remove classes where isDeadClass(name) == true
    Patch.empty
  }

}
