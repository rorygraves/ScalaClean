package scalaclean.cli

import java.io.File

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalaclean.rules.finaliser.Finaliser

class FinaliserProjectTestRunner(projectNames: List[String], runOptions: SimpleRunOptions)
    extends AbstractProjectTestRunner(projectNames, runOptions) {

  override def taskName: String = SCOptions.finaliserCmd

  override def createModelTaskFn(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule = {

    def fn(model: ProjectModel): AbstractRule = {
      new Finaliser(model, runOptions)
    }
    fn
  }

}
