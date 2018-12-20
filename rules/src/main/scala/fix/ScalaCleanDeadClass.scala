package fix

import scalaclean.model.SCModel
import scalafix.v1._

/**
  * A rule that removes unreferenced classes,
  * needs to be run after ScalaCleanAnalysis
  */
class ScalaCleanDeadClass extends SemanticRule("ScalaCleanDeadClass")  {
  val model = new SCModel()


  override def beforeStart(): Unit = {
    // TODO - where do we get the config path to load from - check other rules for examples
    println("Cleaner Rule BEFORE START - load model from config?")

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
