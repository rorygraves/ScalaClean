package scalaclean.cli

import java.io.File
import java.nio.file.Path

import org.kohsuke.args4j.spi.{ MultiPathOptionHandler, StopOptionHandler }
import org.kohsuke.args4j.{ Option => ArgOption }
import java.util.{ List => JList }

import scalaclean.rules.RuleRun

import scala.collection.JavaConverters._

trait RunOptions {
  def addComments: Boolean
  def debug: Boolean
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

abstract class ScalaCleanCommandLine extends RunOptions {

  @ArgOption(
    name = "--files",
    usage = "<file>...",
    required = true,
    hidden = false,
    handler = classOf[MultiPathOptionHandler]
  )
  var _paths: JList[Path] = null

  def files: Seq[Path] = _paths.asScala.toList

  @ArgOption(
    name = "--addComments",
    usage = "emit comments to explain decisions taken",
    required = false,
    hidden = false
  )
  var addComments: Boolean = false

  @ArgOption(
    name = "--debug",
    usage = "emit debug information",
    required = false,
    hidden = true
  )
  var debug: Boolean = false

  @ArgOption(
    name = "--replace",
    usage = "replace existing files",
    required = false,
    hidden = false
  )
  var replace: Boolean = false

  //test specific options, so no API interface
  object testOptions {
    /**
      *    Validate the files against an expectation
      */
    var validate: Boolean = false

    /**
      *  expectation file suffix
      */
    var expectationSuffix: String = ""
  }

}
