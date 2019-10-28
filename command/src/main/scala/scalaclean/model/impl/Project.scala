package scalaclean.model.impl

import scalaclean.model._
import java.io.File
import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.Properties
import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.{SymbolInformation}

import scala.meta.internal.symtab.{GlobalSymbolTable, SymbolTable}
import scala.meta.io.{AbsolutePath, Classpath}

object Project {
  import org.scalaclean.analysis.PropertyNames._
  def apply(propsPath: Path, projects: ProjectSet): Project = {
    val props = new Properties()
    println("PropsPath = " + propsPath)
    props.load(Files.newBufferedReader(propsPath))
    val classpathValue = props.getProperty(prop_classpath)
    val outputPath = props.getProperty(prop_outputDir)
    val elementsFilePath = props.getProperty(prop_elementsFile)
    val relationshipsFilePath = props.getProperty(prop_relationshipsFile)
    val extensionsFilePath = props.getProperty(prop_extensionsFile)
    val src = props.getProperty(prop_src)
    val srcBuildBase = props.getProperty(prop_srcBuildBase)
    val srcFiles = props.getProperty(prop_srcFiles,"").split(File.pathSeparatorChar).toSet
    val srcRoots = props.getProperty(prop_srcRoots).split(File.pathSeparatorChar).toList.sortWith((s1,s2) => s1.length > s1.length || s1 < s2).map(AbsolutePath(_))
    println("srcRoots = "  + srcRoots)
    assert(classpathValue ne null, props.keys)
    assert(outputPath ne null, props.keys)

    val classPath = Classpath.apply(classpathValue)

    new Project(projects, classPath, outputPath, src,srcRoots, srcBuildBase, elementsFilePath, relationshipsFilePath, extensionsFilePath, srcFiles)
  }
}

class Project private(
  val projects: ProjectSet, val classPath: Classpath, val outputPath: String, val src: String,val srcRoots: List[AbsolutePath], val srcBuildBase : String,
  elementsFilePath: String, relationshipsFilePath: String, extensionsFilePath: String,
  val srcFiles: Set[String]) {
  def symbolTable: SymbolTable = GlobalSymbolTable(classPath, includeJdk = true)
  lazy val classloader: ClassLoader = new URLClassLoader(Array(new URL("file:" + outputPath +"/")), null)

  private val infos = new ConcurrentHashMap[ElementId, SymbolInformation]()

  def symbolInfo(viewedFrom: ElementModelImpl, symbol: ElementId): SymbolInformation = {
    infos.computeIfAbsent(symbol,
      s => //any doc in the project would do though
        viewedFrom.source.doc.info(s.symbol).orNull)
  }

  def read = ModelReader.read(this, elementsFilePath, relationshipsFilePath, extensionsFilePath)

  private val sourcesMap = new ConcurrentHashMap[String, SourceData]()

  def source(name: String) = {
    sourcesMap.computeIfAbsent(name, p => new SourceData(this, Paths.get(p)))
  }

}


