package scalaclean.cli

import java.io.File

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalaclean.rules.privatiser.Privatiser

class PrivatiserProjectTestRunner(
    projectNames: List[String],
    runOptions: SimpleRunOptions)
  extends AbstractProjectTestRunner(projectNames, runOptions) {

  override def taskName: String = SCOptions.privatiserCmd
  
  override def createModelTaskFn(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule = {

    def fn(model: ProjectModel): AbstractRule = {
      new Privatiser(model, runOptions)
    }
    fn
  }
}
