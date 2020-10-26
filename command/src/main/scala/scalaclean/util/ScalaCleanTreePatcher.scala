package scalaclean.util

import scalaclean.model.{ ModelElement, SCPatch, SourceModel }
import scalafix.v1.SyntacticDocument

import scala.collection.mutable.ListBuffer

/**
 * a tree visitor that accumulates patches, and counts some basic stats.
 *
 * It has some utility methods for managing patches
 *
 * It has default handling of source files and non source elements
 */
abstract class ScalaCleanTreePatcher(stats: PatchStats, _syntacticDocument: () => SyntacticDocument)
    extends ElementTreeVisitor {

  protected lazy val syntacticDocument: SyntacticDocument = _syntacticDocument()
  protected def visitSourceFile(s: SourceModel)           = true

  protected def visitNotInSource(modelElement: ModelElement) = true

  protected def visitInSource(modelElement: ModelElement): Boolean
  def debug: Boolean
  def addComments: Boolean

  override protected final def visitElement(modelElement: ModelElement): Boolean = {
    val patchcount = patchCount
    _elementsVisited += 1
    val res = modelElement match {
      case s: SourceModel =>
        _filesVisited += 1
        visitSourceFile(s)
      case _ if modelElement.existsInSource =>
        _sourceElementsVisited += 1
        if (addComments)
          addComment(modelElement, s"mark - ${modelElement.mark}")
        visitInSource(modelElement)
      case _ =>
        visitNotInSource(modelElement)
    }
    if (patchcount != patchCount)
      _elementsChanged += 1
    res
  }

  private var _filesVisited          = 0
  private var _elementsVisited       = 0
  private var _sourceElementsVisited = 0
  private var _elementsChanged       = 0

  def filesVisited          = _filesVisited
  def elementsVisited       = _elementsVisited
  def sourceElementsVisited = _sourceElementsVisited
  def elementsChanged       = _elementsChanged

  lazy val tokens = syntacticDocument.tokens.tokens

  def remove(element: ModelElement, comment: String = ""): Unit = {
    replace(element, "", "remove", comment)
  }

  def replace(element: ModelElement, text: String, actionName: String = "replace", comment: String = ""): Unit = {
    if (debug)
      log(s" $actionName(${element.name},'$text')")

    val start =
      if (element.annotations.isEmpty) element.rawStart
    else element.annotations.minBy(_.posOffsetStart).posOffsetStart - 1 + element.rawStart
    val candidateBeginToken = tokens.find(t => t.start >= start && t.start <= t.end).head
    val newBeingToken       = TokenHelper.whitespaceOrCommentsBefore(candidateBeginToken, syntacticDocument.tokens)
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

  final def result: List[SCPatch] = collector.toList.sortBy(_.startPos)

  protected def patchCount = collector.size

  override def afterSource(source: SourceModel): Unit = stats.addFrom(this)
}
