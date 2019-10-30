package scalaclean.cli

import java.io.File
import scopt.OParser

case class SCOptions(
  mode: String = "",
  debug: Boolean = false,
  validate: Boolean = false,
  replace: Boolean = false,
  files: Seq[File] = Seq(),
)

object SCOptions {
  val deadCodeCmd = "deadcode"
  val privatiserCmd = "privatiser"

  val builder = OParser.builder[SCOptions]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("DeadCodeMain"),
      help("help").text("prints this usage text"),
      cmd(deadCodeCmd)
        .required()
        .action( (_, c) => c.copy(mode = "deadcode") )
        .text("Run Dead code").children(
        opt[Unit]("debug")
          .action((_, c) => c.copy(debug = true))
          .text("this option is hidden in the usage text"),
        opt[Unit]("validate (used in development)")
          .action((_, c) => c.copy(validate = true))
          .text("Validate the files against an expectation"),
        opt[Unit]("replace")
          .action((_, c) => c.copy(replace = true))
          .text("Replace the content of the file with the results"),
        arg[File]("<file>...")
          .unbounded()
          .required()
          .action((x, c) => c.copy(files = c.files :+ x))
          .text("target ScalaClean properties files for target projects"),
      ),
      cmd(privatiserCmd)
        .required()
        .action( (_, c) => c.copy(mode = "privatiser") )
        .text("Run Privatiser").children(
        opt[Unit]("debug")
          .action((_, c) => c.copy(debug = true))
          .text("this option is hidden in the usage text"),
        opt[Unit]("validate (used in development)")
          .action((_, c) => c.copy(validate = true))
          .text("Validate the files against an expectation"),
        opt[Unit]("replace")
          .action((_, c) => c.copy(replace = true))
          .text("Replace the content of the file with the results"),
        arg[File]("<file>...")
          .unbounded()
          .required()
          .action((x, c) => c.copy(files = c.files :+ x))
          .text("target ScalaClean properties files for target projects"),
      ),
    )
  }

  def parseCommandLine(args: Array[String]): Option[SCOptions] = {
    // OParser.parse returns Option[Config]
    OParser.parse(parser1, args, SCOptions()) match {
      case Some(config) =>
        if(config.mode == "") {
          println(OParser.usage(parser1))
          None
        } else
          Some(config)
      case _ =>
        None
    }

  }
}