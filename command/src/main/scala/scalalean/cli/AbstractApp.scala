package scalalean.cli

import java.nio.file.Files

import org.kohsuke.args4j.CmdLineParser
import scalaclean.Analysis
import scalafix.scalaclean.FixUtils
import scalafix.v1.SemanticDocument

class AbstractApp[T <: BasicCli: Manifest](cliBean: T) extends App {
  lazy val cli: T = {
    import org.kohsuke.args4j.CmdLineException

    val parser = new CmdLineParser(cliBean)

    try { // parse the arguments.
      parser.parseArgument(args: _*)
      cliBean
    } catch {
      case e: CmdLineException =>
        // if there's a problem in the command line,
        // you'll get this exception. this will report
        // an error message.
        System.err.println(e.getMessage)
        parser.printUsage(System.err)
        System.err.println()
        System.exit(-1)
        ???
    }
  }
  def init(): Unit = {
    val analysis = new Analysis
    def processFile(file: java.io.File) = {
      val res = List.newBuilder[SemanticDocument]
      import scala.collection.JavaConverters._
      val root = file.toPath
      Files.walk(root).filter { f =>
        val name = f.getFileName.toString
        Files.isRegularFile(f) && (name.endsWith(".java") ||name.endsWith(".scala") )
      }.forEach{
        src =>
          println(root)
          println(src)
          res += FixUtils.loadSemanticDoc(root, src)
      }
      res.result()
    }
    analysis.beforeStart()
    cli.otherAnalysedFiles foreach processFile
    cli.inputs foreach processFile



    analysis.afterComplete()

  }
}

import java.io.File

import org.kohsuke.args4j.{Option => cliOption}
class BasicCli {
  @cliOption(name="--analyse", required = true)
  private var inputFiles: Array[File] = null
  def inputs = inputFiles.toList

  @cliOption(name="--library") private
  var otherFiles: Array[File] = Array()
  def otherAnalysedFiles = otherFiles.toList

}
