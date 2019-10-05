package scalaclean.cli

import java.io.File
import scopt.OParser

case class SCOptions(
  debug: Boolean = false,
  files: Seq[File] = Seq(),
)

object SCOptions {
  val builder = OParser.builder[SCOptions]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("DeadCodeMain"),
      opt[Unit]("debug")
        .action((_, c) => c.copy(debug = true))
        .text("this option is hidden in the usage text"),
      help("help").text("prints this usage text"),
      arg[File]("<file>...")
        .unbounded()
        .required()
        .action((x, c) => c.copy(files = c.files :+ x))
        .text("target ScalaClean properties files for target projects"),
    )
  }

  def parseCommandLine(args: Array[String]): Option[SCOptions] = {
    // OParser.parse returns Option[Config]
    OParser.parse(parser1, args, SCOptions()) match {
      case Some(config) =>
        Some(config)
      case _ =>
        None
    }

  }
}