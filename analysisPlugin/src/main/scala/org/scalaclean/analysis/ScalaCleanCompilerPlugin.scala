package org.scalaclean.analysis

import java.nio.file.{Files, Path, Paths}

import org.scalaclean.analysis.plugin.{AnnotationPlugin, ExtensionPluginFactory, ModsPlugin}

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class ScalaCleanCompilerPlugin(override val global: Global) extends Plugin {

  override val name: String        = "scalaclean-analysis-plugin"
  override val description: String = "ScalaClean analysis plugin"

  val component = new ScalaCompilerPluginComponent(global)

  //hardcoded for the moment
  component.extensions += ModsPlugin.create(component, "")
  component.extensions += AnnotationPlugin.create(component, "")

  override def init(options: List[String], error: String => Unit): Boolean = {

    import scala.reflect.runtime.universe
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)

    val realOptions = options.distinct
    component.options = realOptions
    var sourcesPath = Option.empty[List[Path]]

    for (option <- realOptions) {
      if (option == "debug:true") {
        component.debug = true
      } else if (option == "copySources:true") {
        component.copySources = true
      } else if (option.startsWith("extension:")) {
        val end = {
          val end = option.indexOf(':', 10)
          if (end == -1) option.length else end
        }
        val fqn    = option.substring(10, end)
        val module = runtimeMirror.staticModule(fqn)
        runtimeMirror.reflectModule(module).instance match {
          case valid: ExtensionPluginFactory => component.extensions += valid.create(component, option.substring(end))
          case null                          => throw new IllegalArgumentException("not a valid Extension FQN - expected the name of an object")
          case invalid =>
            throw new IllegalArgumentException(
              s"not a valid Extension FQN - ${invalid.getClass.getName} is not a ${classOf[ExtensionDescriptor[_]].getName}"
            )
        }
      } else if (option.startsWith("srcdirs:")) {
        assert (sourcesPath.isEmpty, s"exactly one of specify srcdirs and outputparent should be defined in params $options")
        // Filter out source dirs passed in which don't actually exist
        val sourceDirsInArgument = option.substring(8).split(java.io.File.pathSeparatorChar).toList
        val filteredSourceDirs   = sourceDirsInArgument.collect{
          case srcDir if Files.exists(Paths.get(srcDir)) => Paths.get(srcDir)
        }
        assert (filteredSourceDirs.nonEmpty, s"no directories exist! $sourceDirsInArgument")
        sourcesPath = Some(filteredSourceDirs)
      } else if (option.startsWith("outputparent:")) {
        assert (sourcesPath.isEmpty, s"exactly one of specify srcdirs and outputparent should be defined in params $options")
        val parentDir = Paths.get(option.substring(13))
        Files.createDirectories(parentDir)
        sourcesPath = Some(List())
        component.outputParent = Some(parentDir)
      } else
        error(s"Option not recognised: $option")
    }
    require (sourcesPath.isDefined, s"must specify one of srcdirs and outputparent in params $options")
    component.sourcesRoot = sourcesPath.get

    true
  }

  override val optionsHelp: Option[String] = Some(
    s"""-P:$name:debug:true        Set debugging on the ScalaClean analysis plugin
       |-P:$name:srcdirs           The path of sources, separated by ${java.io.File.pathSeparatorChar}
       |-P:$name:outputparent      The path to outputs parent directory
       |-P:$name:extension:<fqn>   Add an extension dataset. FQN is the fully qualified name of the appropriate ExtensionDescriptor object
       |                Exactly one of outputparent and srcdirs muct be specified
       |""".stripMargin
  )

  override val components: List[PluginComponent] = List(
    component
  )
}
