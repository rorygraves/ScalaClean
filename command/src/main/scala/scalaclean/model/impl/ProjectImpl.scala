package scalaclean.model.impl

import java.io.File
import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.Properties
import java.util.concurrent.ConcurrentHashMap
import java.util.jar.JarFile

import scalaclean.model.{AllProjectsModel, SingleProjectModel, SourceModel}

import scala.meta.internal.symtab.{GlobalSymbolTable, SymbolTable}
import scala.meta.io.{AbsolutePath, Classpath}

object ProjectImpl {

  import org.scalaclean.analysis.PropertyNames._

  def apply(propsPath: Path, projects: ProjectSet): ProjectImpl = this.synchronized {
    val props = new Properties()
    println("PropsPath = " + propsPath)
    props.load(Files.newBufferedReader(propsPath))

    val sourcePathSep = props.getProperty(prop_sourceOsPathSeparator)
    val sourceDirSep  = props.getProperty(prop_sourceOsDirSeparator)

    val classpathValue =
      props.getProperty(prop_classpath).replace(sourcePathSep, File.pathSeparator).replace(sourceDirSep, File.separator)
    val outputPath            = props.getProperty(prop_outputDir).replace(sourceDirSep, File.separator)
    val elementsFilePath      = props.getProperty(prop_elementsFile).replace(sourceDirSep, File.separator)
    val relationshipsFilePath = props.getProperty(prop_relationshipsFile).replace(sourceDirSep, File.separator)
    val extensionsFilePath    = props.getProperty(prop_extensionsFile).replace(sourceDirSep, File.separator)
    val srcJarPath            = propsPath.resolveSibling("sources.jar")
    val srcBuildBase          = props.getProperty(prop_srcBuildBase).replace(sourceDirSep, File.separator)
    val srcFiles =
      props.getProperty(prop_srcFiles, "").replace(sourceDirSep, File.separator).split(File.pathSeparatorChar).toSet
    val srcRoots = props
      .getProperty(prop_srcRoots)
      .replace(sourceDirSep, File.separator)
      .split(sourcePathSep)
      .toList
      .sortWith((s1, s2) => s1.length > s1.length || s1 < s2)
      .map(AbsolutePath(_))
    println("srcRoots = " + srcRoots)
    assert(classpathValue ne null, props.keys)
    assert(outputPath ne null, props.keys)

    val classPath = Classpath(classpathValue)

    new ProjectImpl(
      projects,
      classPath,
      outputPath,
      srcRoots,
      srcBuildBase,
      elementsFilePath,
      relationshipsFilePath,
      extensionsFilePath,
      srcJarPath,
      srcFiles,
      sourceDirSep
    )
  }

}

class ProjectImpl private(
    val projects: ProjectSet,
    val classPath: Classpath,
    val outputPath: String,
    val srcRoots: List[AbsolutePath],
    val srcBuildBase: String,
    elementsFilePath: String,
    relationshipsFilePath: String,
    extensionsFilePath: String,
    sourcesFilePath: Path,
    val srcFiles: Set[String],
    sourceDirSep: String
) extends SingleProjectModel {
  def symbolTable: SymbolTable = GlobalSymbolTable(classPath, includeJdk = true)

  lazy val classloader: ClassLoader = new URLClassLoader(Array(new URL("file:" + outputPath + "/")), null)

  def read: (Vector[ElementModelImpl], BasicRelationshipInfo) =
    ModelReader.read(this, elementsFilePath, relationshipsFilePath, extensionsFilePath, sourceDirSep)

  private val sourcesMap = new ConcurrentHashMap[String, SourceData]()

  def source(name: String): SourceData = {
    sourcesMap.computeIfAbsent(name, p => SourceData(this, Paths.get(p)))
  }

  lazy val sourcesJar = {
    val path = sourcesFilePath
    if (Files.exists(path)) {
      Some(new JarFile(path.toFile, false))
    } else None
  }
  override def originalSource(file: SourceModel): Option[String] = {
    sourcesJar map { jar =>
      assert(file.filename.isAbsolute, file.filename.toString)
      val name = file.filename.getRoot.relativize(file.filename.toRealPath()).toString
      val entry = jar.getEntry(name)
      val data = new Array[Byte](entry.getSize.toInt)
      val is = jar.getInputStream(entry)
      var offset = 0
      while (data.length - offset > 0) {
        val read = is.read(data, offset, data.length - offset)
        assert (read > 0)
        offset += read
      }
      new String(data, file.encoding)
    }
  }

  override def allProjects: AllProjectsModel = projects
}
