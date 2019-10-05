package scalaclean.cli

import scalaclean.rules.privatiser.Privatiser

object PrivatiserMain {
  def main(args: Array[String]): Unit = {
    SCOptions.parseCommandLine(args) match {
      case Some(options) =>
        new ScalaCleanMain(options, model =>  new Privatiser(model)).run()
      case None =>
        System.exit(0)
    }
  }
}