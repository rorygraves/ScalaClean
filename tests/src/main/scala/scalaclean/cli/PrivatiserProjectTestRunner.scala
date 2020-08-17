package scalaclean.cli

import java.io.File

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalaclean.rules.privatiser.Privatiser

class PrivatiserProjectTestRunner(projectNames: List[String], overwriteTargetFiles: Boolean) extends AbstractProjectTestRunner(projectNames, overwriteTargetFiles) {

  def taskName = SCOptions.privatiserCmd

  def createRule(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule = {
    new Privatiser(_, debug)
  }
}
