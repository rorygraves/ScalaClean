package scalaclean.cli

import java.nio.file.{Files, Path, Paths}

import org.kohsuke.args4j.spi.{MultiPathOptionHandler, OneArgumentOptionHandler, Setter}
import org.kohsuke.args4j.{CmdLineParser, OptionDef, Option => ArgOption}
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
    handler = classOf[OptionMultiPathOptionHandler]
  )
  var _paths = Option.empty[List[Path]]

  @ArgOption(
    name = "--filesRoot",
    usage = "<dir> with the scala clean property files in child directories",
    required = false,
    hidden = false,
    forbids = Array("--files"),
    handler = classOf[OptionPathOptionHandler]
  )
  var _root = Option.empty[Path]

  @ArgOption(
    name = "--filesRootDepth",
    usage = "how deep to scan --fileRoot for files. Default Int.MaxValue",
    required = false,
    hidden = false,
    depends = Array("--filesRoot"),
    forbids = Array("--files")
  )
  var filesRootDepth = Int.MaxValue

  def files: Seq[Path] = {
    (_root, _paths) match {
      case (None, None) =>
        throw new IllegalArgumentException("either '--files' or '--fileRoot' must be specified")
      case (_: Some[_], _: Some[_]) =>
        throw new IllegalArgumentException("only one of '--files' or '--fileRoot' must be specified")
      case (Some(root), _) =>
        Files.find(root, filesRootDepth, (path, _) => path.endsWith("ScalaClean.properties")).iterator().asScala.toList
      case (_, Some(paths)) =>
        paths
    }
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
    name = "--maxIssues",
    usage = "maximum number of issues before aborting",
    required = false,
    hidden = false
  )
  var maxIssues = 0

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


  @ArgOption(
    name = "--skipNonexistentFiles",
    usage = "skip files that are not found",
    required = false,
    hidden = false
  )
  var skipNonexistentFiles: Boolean = false
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
class OptionPathOptionHandler(parser: CmdLineParser, option: OptionDef, setter: Setter[_ >: Some[Path]])
  extends OneArgumentOptionHandler[Some[Path]] (parser, option, setter) {
  override def parse(argument: String): Some[Path] = Some(Paths.get(argument))
}
class OptionMultiPathOptionHandler(parser: CmdLineParser, option: OptionDef, setter: Setter[_ >: Some[List[Path]]])
  extends OneArgumentOptionHandler[Some[List[Path]]] (parser, option, setter) {
  private def sysPathSeperator: String = System.getProperty("path.separator")
  override def parse(argument: String): Some[List[Path]] = {
    val params = argument.split(sysPathSeperator).toList
    Some(params.map(Paths.get(_)))
  }
}

