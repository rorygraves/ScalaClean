package scalaclean.cli

import java.io.File

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalaclean.rules.deadcode.DeadCodeRemover

class DeadCodeProjectTestRunner(
  projectNames: List[String],
  overwriteTargetFiles: Boolean) extends AbstractProjectTestRunner(projectNames, overwriteTargetFiles) {

  def taskName = SCOptions.deadCodeCmd

  def createRule(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule = {
    new DeadCodeRemover(_, debug)
  }
}
