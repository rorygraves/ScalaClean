package scalaclean.rules.privatiser

import scalaclean.model.ProjectModel
import scalaclean.rules.AbstractRule

object FullPrivatiser extends AbstractRule[FullPrivatiserCommandLine] {
  override type Rule = FullPrivatiser
  override def cmdLine                                                        = new FullPrivatiserCommandLine
  override def apply(options: FullPrivatiserCommandLine, model: ProjectModel) = new Rule(options, model)
}

class FullPrivatiser(options: FullPrivatiserCommandLine, model: ProjectModel) extends AbstractPrivatiser(options, model)
class FullPrivatiserCommandLine                                               extends AbstractPrivatiserCommandLine
