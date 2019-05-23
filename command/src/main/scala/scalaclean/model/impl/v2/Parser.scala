package scalaclean.model.impl.v2

import java.nio.file.{Files, Path, Paths}
import java.util.Properties

import scalaclean.model._
import scalafix.v1.{SemanticDocument, Symbol}

import scala.collection.mutable

class ParserImpl extends ParseModel {

  def analyse(implicit doc: SemanticDocument) = new ParserTreeWalker(this, doc).analyse
  def finishedParsing(): Unit = {}

  def writeToFile(path:Path, projectsSource: Path): Unit = {
    val writer = new ParsedWriter(path, projectsSource)
    bySymbol.values.foreach (writer.writeElement)
    writer.close()
  }

  override def asProjectModel(storagePath: String, projectName: String, classpath: String, outputDir: String, relSource: String, absSource: String): ProjectModel = {


    val projectsRoot = Paths.get(storagePath)
    val projectPath = Paths.get(relSource) // privatiser-test-input/src/main/scala

    writeToFile(projectPath, projectsRoot)

    val props = new Properties
    props.put("classpath", classpath)
    props.put("outputDir", outputDir)
    props.put("src", absSource)

    val propsFile = projectsRoot.resolve(s"$projectName.properties")
    println("Writing props file " + propsFile)
    props.store(Files.newBufferedWriter(propsFile),"")
    new Projects(projectsRoot,(projectName, projectPath))
  }

  private[v2] val internalAccess = new InternalAccess()
  private[v2] val additionalDataBuilder = new ExtraParsedData(this)


  private val bySymbol = mutable.Map[Symbol, ParsedElement]()
  private[v2] def record( element: ParsedElement): Unit = {
    assert(bySymbol.put(element.symbol, element).isEmpty, s"${element.symbol} enclosing ${element.enclosing} and =$element")
  }

}