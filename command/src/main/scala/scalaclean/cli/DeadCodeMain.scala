package scalaclean.cli

import scalaclean.rules.deadcode.DeadCodeRemover


object DeadCodeMain {
  def main(args: Array[String]): Unit = {
    SCOptions.parseCommandLine(args) match {
      case Some(options) =>
        new ScalaCleanMain(options, model =>  new DeadCodeRemover(model)).run()
      case None =>
        System.exit(0)
    }
  }
}

