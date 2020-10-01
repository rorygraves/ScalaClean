package scalaclean.cli

import java.io.File

import scopt.OParser

trait RunOptions {
  val addComments: Boolean
  val debug: Boolean
  val replace: Boolean
}

case class SimpleRunOptions(
    debug: Boolean = false,
    addComments: Boolean = false,
    replace: Boolean = false,
) extends RunOptions {

  def orReplace(replace: Boolean): SimpleRunOptions = {
    if (replace) copy(replace = replace) else this
  }

}

case class SCOptions(
    mode: String = "",
    debug: Boolean = false,
    addComments: Boolean = false,
    validate: Boolean = false,
    replace: Boolean = false,
    files: Seq[File] = Seq(),
) extends RunOptions

object SCOptions {
  val deadCodeCmd         = "deadcode"
  val privatiserCmd       = "privatiser"
  val finaliserCmd        = "finaliser"
  val simpleDeadCodeCmd   = "simpledeadcode"
  val simplePrivatiserCmd = "simpleprivatiser"

  val optionsParser: OParser[Unit, SCOptions] = {

    val builder = OParser.builder[SCOptions]
    import builder._

    val sharedOptions = List(
      opt[Unit]("debug")
        .action((_, c) => c.copy(debug = true))
        .text("this option is hidden in the usage text"),
      opt[Unit]("addComments")
        .action((_, c) => c.copy(addComments = true))
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
        .text("target ScalaClean properties files for target projects")
    )

    OParser.sequence(
      programName("ScalaCleanMain"),
      help("help").text("prints this usage text"),
      cmd(deadCodeCmd)
        .required()
        .action((_, c) => c.copy(mode = "deadcode"))
        .text("Run Dead Code")
        .children(sharedOptions: _*),
      cmd(privatiserCmd)
        .required()
        .action((_, c) => c.copy(mode = "privatiser"))
        .text("Run Privatiser")
        .children(sharedOptions: _*),
      cmd(simpleDeadCodeCmd)
        .required()
        .action((_, c) => c.copy(mode = "simpledeadcode"))
        .text("Run simple dead code")
        .children(sharedOptions: _*),
      cmd(simplePrivatiserCmd)
        .required()
        .action((_, c) => c.copy(mode = "simpleprivatiser"))
        .text("Run simple privatiser")
        .children(sharedOptions: _*)
    )
  }

  def parseCommandLine(args: Array[String]): Option[SCOptions] = {
    // OParser.parse returns Option[Config]
    OParser.parse(optionsParser, args, SCOptions()) match {
      case Some(config) =>
        if (config.mode == "") { // they did not supply an option
          println(OParser.usage(optionsParser))
          None
        } else
          Some(config)
      case _ =>
        None
    }

  }

}
