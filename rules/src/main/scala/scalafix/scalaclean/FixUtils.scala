package scalafix.scalaclean

import java.nio.file.{Path, Paths}

import scalaclean.model.impl.v2.Project
import scalafix.internal.v1.InternalSemanticDoc
import scalafix.v1.SemanticDocument.Error
import scalafix.v1.{SemanticDocument, SyntacticDocument}

import scala.meta.inputs.Input
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.symtab.SymbolTable
import scala.meta.io.{AbsolutePath, RelativePath}

object FixUtils {

 def loadSyntaticDocument(project: Project, path: Path): SyntacticDocument = {
    SyntacticDocument.fromInput(Input.File(AbsolutePath(Paths.get(project.src).resolve(path))))
  }
  def loadSemanticDoc(project: Project, path: Path) : SemanticDocument = {
    val syntacticDoc = loadSyntaticDocument(project, path)
    val rel = if (path.isAbsolute) project.absolutePath.relativize(path) else path
    println(rel)
    println(project.relativePath)
    println(project.relativePath.resolve(rel))
    fromPath(syntacticDoc, RelativePath(project.relativePath.resolve(rel)), project.classloader, project.symbolTable)
  }

  private[scalafix] def fromPath(
                                  doc: SyntacticDocument,
                                  path: RelativePath,
                                  classLoader: ClassLoader,
                                  symtab: SymbolTable
                                ): SemanticDocument = {
    import scala.collection.JavaConverters._
    val reluri = path.toNIO.iterator().asScala.mkString("/")
    val semanticdbReluri = s"META-INF/semanticdb/$reluri.semanticdb"
    Option(classLoader.getResourceAsStream(semanticdbReluri)) match {
      case Some(inputStream) =>
        val sdocs =
          try TextDocuments.parseFrom(inputStream).documents
          finally inputStream.close()
        val sdoc = sdocs.find(_.uri == reluri).getOrElse {
          throw Error.MissingTextDocument(reluri)
        }
        val impl = new InternalSemanticDoc(doc, sdoc, symtab)
        new SemanticDocument(impl)
      case None =>
        throw Error.MissingSemanticdb(semanticdbReluri)
    }
  }

}
