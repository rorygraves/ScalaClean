package scalaclean.rules.deadcode

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule

object FullDeadCodeRemover extends AbstractRule[FullDeadCodeCommandLine] {

  override type Rule = FullDeadCodeRemover

  override def cmdLine                                                      = new FullDeadCodeCommandLine
  override def apply(options: FullDeadCodeCommandLine, model: ProjectModel) = new Rule(options, model)
}

class FullDeadCodeRemover(override val options: FullDeadCodeCommandLine, override val model: ProjectModel)
    extends AbstractDeadCodeRemover[FullDeadCodeCommandLine]

class FullDeadCodeCommandLine extends AbstractDeadCodeCommandLine
