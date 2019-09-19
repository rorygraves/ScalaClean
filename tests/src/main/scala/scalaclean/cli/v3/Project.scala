package scalaclean.cli.v3

import java.net.{URL, URLClassLoader}
import java.nio.file.{Path, Paths}
import java.util.Properties
import java.util.concurrent.ConcurrentHashMap

import scalaclean.model.{ModelElement, ProjectModel}
import scalafix.v1.{Symbol, SymbolInformation}

import scala.collection.immutable
import scala.meta.internal.symtab.SymbolTable
import scala.reflect.ClassTag

class Project(name: String, path: Path, val projects: Projects) {
  val (classPath, outputPath, src) = {
    val props = new Properties()
    val propsPath = projects.projectRoot.resolve(s"$name.properties")
//    println("PropsPath = " + propsPath)
//    props.load(Files.newBufferedReader(propsPath))
//    val cp = props.getProperty("classpath")
//    val output = props.getProperty("outputDir")
//    val src = props.getProperty("src")
//    assert (cp ne null, props.keys)
//    assert (output ne null, props.keys)
//    (Classpath.apply(cp), output, src)
    (null,null, null)
  }
  def symbolTable: SymbolTable = null //GlobalSymbolTable(classPath, true)

  println("OUTPUTPATH = " + outputPath)
  lazy val classloader: ClassLoader = new URLClassLoader(Array(new URL("file:"+outputPath)), null)

  def relativePath = if (path.isAbsolute) path.relativize((projects.projectRoot)) else path
  def absolutePath = projects.projectRoot.resolve(path).toAbsolutePath
  private val infos = new ConcurrentHashMap[Symbol, SymbolInformation]()
  def symbolInfo(viewedFrom: ElementModelImpl, symbol: Symbol): SymbolInformation = {
    infos.computeIfAbsent(symbol,
      s => //any doc in the project would do though
        viewedFrom.source.doc.info(s).orNull)
  }

  def read = ModelReader.read(this, path, projects.projectRoot)

  private val sourcesMap = new ConcurrentHashMap[String, SourceData]()
  def source(name:String) = {
    sourcesMap.computeIfAbsent(name, p => new SourceData(this, Paths.get(p)))
  }

}

class Projects(val projectRoot: Path, projectPaths: (String,Path) *) extends ProjectModel {
  val projects = projectPaths.toList map { p => new Project(p._1, p._2, this)}

  val elements: Map[Symbol, ElementModelImpl] = {
    val (elements, rels: immutable.Seq[BasicRelationshipInfo]) = projects.map(_.read) unzip

    val elementsMap = elements.flatten.toVector.map (e => e.symbol -> e) toMap

    val relsFrom = rels.reduce(_ + _)
    val relsTo = relsFrom.byTo

    relsFrom.complete(elementsMap)
    elementsMap.values foreach (_.complete(elementsMap, relsFrom, relsTo))

    elementsMap
  }

  override def fromSymbol[T <: ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): T = {
    val targetSymbol = if (symbol.isGlobal) Symbol("G:" + symbol.value) else {
      elements.keys.find(_.value.contains(symbol.value)).getOrElse {
        throw new IllegalStateException("Unable to match symbol: " + symbol)
      }

    }
    //    elements.get(symbol) match {
    elements.get(targetSymbol) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x: T) => x
      case Some(x) => throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
    }
  }


  override def getSymbol[T <: ModelElement](symbol: Symbol)(implicit tpe: ClassTag[T]): Option[T] = {
    elements.get(symbol) match {
      case None => None
      case Some(x: T) => Some(x)
      case Some(x) => throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
    }
  }

  override def size: Int = elements.size

  override def allOf[T <: ModelElement : ClassTag]: Iterator[T] = {
    elements.values.iterator collect{
      case x:T => x
    }
  }

  def save: Unit = ???
}
