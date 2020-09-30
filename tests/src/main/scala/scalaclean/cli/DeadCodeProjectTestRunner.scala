package scalaclean.cli

import java.io.File

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalaclean.rules.deadcode.DeadCodeRemover

class DeadCodeProjectTestRunner(
    projectNames: List[String],
    runOptions: SimpleRunOptions)
  extends AbstractProjectTestRunner(projectNames, runOptions) {

  override def taskName: String = SCOptions.deadCodeCmd

  override def createModelTaskFn(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule = {

    def fn(model: ProjectModel): AbstractRule = {
      new DeadCodeRemover(model, runOptions)
    }

    fn
  }
}
