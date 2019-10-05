package scalaclean.model.v3

import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.Properties
import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.{Symbol, SymbolInformation}

import scala.meta.internal.symtab.{GlobalSymbolTable, SymbolTable}
import scala.meta.io.Classpath

object Project {
  def apply(propsPath: Path, projects: ProjectSet): Project = {
    val props = new Properties()
    println("PropsPath = " + propsPath)
    props.load(Files.newBufferedReader(propsPath))
    val classpathValue = props.getProperty("classpath")
    val outputPath = props.getProperty("outputDir")
    val elementsFilePath = props.getProperty("elementsFile")
    val relationshipsFilePath = props.getProperty("relationshipsFile")
    val src = props.getProperty("src")
    assert(classpathValue ne null, props.keys)
    assert(outputPath ne null, props.keys)

    val classPath = Classpath.apply(classpathValue)

    println("STEP 1 = rels = " +relationshipsFilePath)

    new Project(projects, classPath, outputPath, src,elementsFilePath, relationshipsFilePath)
  }
}

class Project private(
  val projects: ProjectSet, val classPath: Classpath, val outputPath: String, val src: String,
  elementsFilePath: String, relationshipsFilePAth: String) {
  def symbolTable: SymbolTable = GlobalSymbolTable(classPath, includeJdk = true)

  lazy val classloader: ClassLoader = new URLClassLoader(Array(new URL("file:" + outputPath)), null)

  private val infos = new ConcurrentHashMap[Symbol, SymbolInformation]()

  def symbolInfo(viewedFrom: ElementModelImpl, symbol: Symbol): SymbolInformation = {
    infos.computeIfAbsent(symbol,
      s => //any doc in the project would do though
        viewedFrom.source.doc.info(s).orNull)
  }

  def read = ModelReader.read(this, projects.projectRoot, elementsFilePath, relationshipsFilePAth)

  private val sourcesMap = new ConcurrentHashMap[String, SourceData]()

  def source(name: String) = {
    sourcesMap.computeIfAbsent(name, p => new SourceData(this, Paths.get(p)))
  }

}


