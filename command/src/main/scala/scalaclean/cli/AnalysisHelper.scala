package scalaclean.cli

import java.io.File

import scalaclean.Analysis
import scalafix.internal.reflect.ClasspathOps
import scalafix.scalaclean.cli.DocHelper

import scala.meta.{AbsolutePath, Classpath, RelativePath}

object AnalysisHelper {
  def runAnalysis(projectName: String,inputClasspath: Classpath, sourceRoot: AbsolutePath,
    inputSourceDirectories: List[AbsolutePath], outputClassDir: String, storagePath: String, targetFiles: List[RelativePath]): Unit = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)

      // run analysis
    val analysis = new Analysis(storagePath, projectName,inputClasspath.entries.map(_.toString()).mkString(File.pathSeparator),
      outputClassDir,inputSourceDirectories.head.toRelative(sourceRoot).toString(),inputSourceDirectories.head.toString())

    targetFiles.foreach { targetFile =>
      println(s"Processing file: $targetFile")
      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, inputSourceDirectories.head, sourceRoot, targetFile)
      analysis.analyse(sdoc)
    }
    analysis.afterComplete()
  }
}
