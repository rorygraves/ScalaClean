package scalaclean.cli

import java.io.File

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule
import scalaclean.rules.deadcode.SimpleDeadCode

class SimpleDeadCodeProjectTestRunner(projectNames: List[String], runOptions: SimpleRunOptions)
    extends AbstractProjectTestRunner(projectNames, runOptions) {

  override def taskName: String = SCOptions.simpleDeadCodeCmd

  def createModelTaskFn(propsFiles: Seq[File], debug: Boolean): ProjectModel => AbstractRule = {
    def fn(model: ProjectModel): AbstractRule = {
      new SimpleDeadCode(model, runOptions)
    }
    fn
  }

}
