package scalaclean.cli

import java.nio.file.{Files, Path, Paths}

import org.kohsuke.args4j.spi.{MultiPathOptionHandler, PathOptionHandler}
import org.kohsuke.args4j.{Option => ArgOption}
import java.util.{Collections, List => JList}

import scalaclean.rules.plugin.{RulePlugin, RulePluginFactory}

import scala.collection.JavaConverters._

case class SimpleRunOptions(
    debug: Boolean = false,
    addComments: Boolean = false,
    replace: Boolean = false,
    rulePluginText: List[String] = Nil) {

  def orReplace(replace: Boolean): SimpleRunOptions = {
    if (replace) copy(replace = replace) else this
  }

}

abstract class ScalaCleanCommandLine {

  @ArgOption(
    name = "--files",
    usage = "<file>...",
    required = false,
    hidden = false,
    forbids = Array("--filesRoot"),
    handler = classOf[MultiPathOptionHandler]
  )
  var _paths: JList[Path] = Collections.emptyList()

  @ArgOption(
    name = "--filesRoot",
    usage = "<dir> with the scala clean property files in child directories",
    required = false,
    hidden = false,
    forbids = Array("--files"),
    handler = classOf[PathOptionHandler]
  )
  var _root: Path = Paths.get("<none>")

  def files: Seq[Path] = {
    if (_root.toString == "<none>" && _paths.isEmpty)
      throw new IllegalArgumentException("one of '--files' or '--fileRoot' must be specified")

    if (_root.toString != "<none>")
      Files.find(_root, 2, (path, _) => path.endsWith("ScalaClean.properties")).iterator().asScala.toList
    else
      _paths.asScala.toList
  }

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

  @ArgOption(
    name = "--rulePlugins",
    usage = "additional rules typically to ban changes. " +
      "This takes multiple args in the form --rulePlugins <objectname>:params [<objectname>:params ]*. " +
      "objectname should be the name of a scala object extending scalaclean.rules.plugin.RulePluginFactory",
    required = false,
    handler = classOf[MultiPathOptionHandler],
    hidden = false
  )
  var _rulePlugins: JList[String] = Collections.emptyList()

  def rulePlugins: Seq[RulePlugin] =
    _rulePlugins.asScala.map { objectNameAndParams =>
      try {
        RulePluginFactory(objectNameAndParams)
      } catch {
        case e: Exception => throw new Exception(s"failed to parse parameter '$objectNameAndParams'", e)
      }
    }.toList

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
