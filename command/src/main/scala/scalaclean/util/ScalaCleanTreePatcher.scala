package scalaclean.util

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model.{ModelElement, SCPatch, SourceModel}
import scalaclean.rules.SourceFile
import scalafix.v1.SyntacticDocument

import scala.collection.mutable.ListBuffer

/**
 * a tree visitor that accumulates patches, and counts some basic stats.
 *
 * It has some utility methods for managing patches
 *
 * It has default handling of source files and non source elements
 */
abstract class ScalaCleanTreePatcher[O <: ScalaCleanCommandLine](sourceFile: SourceFile, options: O)
  extends ElementTreeVisitor[O](sourceFile, options) {

  protected def syntacticDocument: SyntacticDocument = sourceFile.syntacticDocument
  def tokenArray = sourceFile.tokenArray

  protected def visitSourceFile(s: SourceModel)      = true

  protected def visitNotInSource(modelElement: ModelElement) = true

  protected def visitInSource(modelElement: ModelElement): Boolean

  override protected final def visitElement(modelElement: ModelElement): Boolean = {
    elementsVisited += 1
    val res = modelElement match {
      case s: SourceModel =>
        visitSourceFile(s)
      case _ if modelElement.existsInSource =>
        sourceElementsVisited += 1
        if (addComments)
          addComment(modelElement, s"mark - ${modelElement.mark}")
        visitInSource(modelElement)
      case _ =>
        visitNotInSource(modelElement)
    }
    res
  }

  private var elementsVisited       = 0
  private var sourceElementsVisited = 0

  def result = SingleFileVisit(collector.toList.sortBy(_.startPos), elementsVisited, sourceElementsVisited)

  def remove(element: ModelElement, comment: String = ""): Unit = {
    replace(element, "", "remove", comment)
  }

  def replace(element: ModelElement, text: String, actionName: String = "replace", comment: String = ""): Unit = {
    if (debug)
      log(s" $actionName(${element.name},'$text') --- $comment")

    val start =
      if (element.annotations.isEmpty) element.rawStart
      else element.annotations.minBy(_.posOffsetStart).posOffsetStart - 1 + element.rawStart
    val candidateBeginToken = tokenArray.find(t => t.start >= start && t.start <= t.end).head
    val newBeingToken       = TokenHelper.whitespaceOrCommentsBefore(candidateBeginToken, tokenArray)
    val newStartPos         = newBeingToken.headOption.map(_.start).getOrElse(start)

    collect(SCPatch(newStartPos, element.rawEnd, text, comment))
  }

  def replaceFromFocus(element: ModelElement, text: String, comment: String): Unit = {
    if (debug)
      log(s" replaceFromFocus(${element.name},'$text')  ${element.rawFocusStart}->${element.rawEnd}")
    collect(SCPatch(element.rawFocusStart, element.rawEnd, text, comment))
  }

  def addComment(element: ModelElement, msg: String, comment: String = ""): Unit = {
    if (debug)
      log(" addComment(" + element.name + ",'" + msg + "')")
    val text = s"/* *** SCALA CLEAN $msg */"
    collect(SCPatch(element.rawStart, element.rawStart, text, comment))
  }

  private val collector = new ListBuffer[SCPatch]()

  final def collect(value: SCPatch): Unit = {
    collector.+=(value)
  }

}

case class SingleFileVisit(patches: List[SCPatch], elementsVisited: Int, sourceElementsVisited: Int)
