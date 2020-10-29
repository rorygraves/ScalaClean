package scalaclean.rules.deadcode

import scalaclean.model.AllProjectsModel
import scalaclean.rules.AbstractRule

object FullDeadCodeRemover extends AbstractRule[FullDeadCodeCommandLine] {

  override type Rule = FullDeadCodeRemover

  override def cmdLine                                                      = new FullDeadCodeCommandLine
  override def apply(options: FullDeadCodeCommandLine, model: AllProjectsModel) = new Rule(options, model)
}

class FullDeadCodeRemover(override val options: FullDeadCodeCommandLine, override val model: AllProjectsModel)
    extends AbstractDeadCodeRemover[FullDeadCodeCommandLine]

class FullDeadCodeCommandLine extends AbstractDeadCodeCommandLine
