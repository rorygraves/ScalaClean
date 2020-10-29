package scalaclean.rules.privatiser

import scalaclean.model.AllProjectsModel
import scalaclean.rules.AbstractRule

object FullPrivatiser extends AbstractRule[FullPrivatiserCommandLine] {
  override type Rule = FullPrivatiser
  override def cmdLine                                                        = new FullPrivatiserCommandLine
  override def apply(options: FullPrivatiserCommandLine, model: AllProjectsModel) = new Rule(options, model)
}

class FullPrivatiser(options: FullPrivatiserCommandLine, model: AllProjectsModel) extends AbstractPrivatiser(options, model)
class FullPrivatiserCommandLine                                               extends AbstractPrivatiserCommandLine
