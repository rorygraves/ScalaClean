package scalaclean.model.impl

import java.io.File
import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.Properties
import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.SymbolInformation

import scala.meta.internal.symtab.{GlobalSymbolTable, SymbolTable}
import scala.meta.io.{AbsolutePath, Classpath}

object Project {

  import org.scalaclean.analysis.PropertyNames._

  def apply(propsPath: Path, projects: ProjectSet): Project = {
    val props = new Properties()
    println("PropsPath = " + propsPath)
    props.load(Files.newBufferedReader(propsPath))

    val sourcePathSep = props.getProperty(prop_sourceOsPathSeparator)
    val sourceDirSep = props.getProperty(prop_sourceOsDirSeparator)

    val classpathValue = props.getProperty(prop_classpath).replace(sourcePathSep, File.pathSeparator).replace(sourceDirSep, File.separator)
    val outputPath = props.getProperty(prop_outputDir).replace(sourceDirSep, File.separator)
    val elementsFilePath = props.getProperty(prop_elementsFile).replace(sourceDirSep, File.separator)
    val relationshipsFilePath = props.getProperty(prop_relationshipsFile).replace(sourceDirSep, File.separator)
    val extensionsFilePath = props.getProperty(prop_extensionsFile).replace(sourceDirSep, File.separator)
    val src = props.getProperty(prop_src).replace(sourceDirSep, File.separator)
    val srcBuildBase = props.getProperty(prop_srcBuildBase).replace(sourceDirSep, File.separator)
    val srcFiles = props.getProperty(prop_srcFiles, "").replace(sourceDirSep, File.separator).split(File.pathSeparatorChar).toSet
    val srcRoots = props.getProperty(prop_srcRoots).replace(sourceDirSep, File.separator).split(sourcePathSep).toList.sortWith((s1, s2) => s1.length > s1.length || s1 < s2).map(AbsolutePath(_))
    println("srcRoots = " + srcRoots)
    assert(classpathValue ne null, props.keys)
    assert(outputPath ne null, props.keys)

    val classPath = Classpath(classpathValue)

    new Project(projects, classPath, outputPath, src, srcRoots, srcBuildBase, elementsFilePath, relationshipsFilePath, extensionsFilePath, srcFiles, sourceDirSep)
  }
}

class Project private(
                       val projects: ProjectSet, val classPath: Classpath, val outputPath: String, val src: String, val srcRoots: List[AbsolutePath], val srcBuildBase: String,
                       elementsFilePath: String, relationshipsFilePath: String, extensionsFilePath: String,
                       val srcFiles: Set[String], sourceDirSep: String) {
  def symbolTable: SymbolTable = GlobalSymbolTable(classPath, includeJdk = true)

  lazy val classloader: ClassLoader = new URLClassLoader(Array(new URL("file:" + outputPath + "/")), null)

  private val infos = new ConcurrentHashMap[ElementId, SymbolInformation]()

  def symbolInfo(viewedFrom: ElementModelImpl, symbol: ElementId): SymbolInformation = {
    infos.computeIfAbsent(symbol,
      s => //any doc in the project would do though
        viewedFrom.source.doc.info(s.symbol).orNull)
  }

  def read: (Vector[ElementModelImpl], BasicRelationshipInfo) = ModelReader.read(this, elementsFilePath, relationshipsFilePath, extensionsFilePath, sourceDirSep)

  private val sourcesMap = new ConcurrentHashMap[String, SourceData]()

  def source(name: String): SourceData = {
    sourcesMap.computeIfAbsent(name, p => SourceData(this, Paths.get(p)))
  }

}


