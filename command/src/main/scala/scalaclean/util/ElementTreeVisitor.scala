package scalaclean.util

import scalaclean.model.{ModelElement, SCPatch}
import scalafix.v1.SyntacticDocument

import scala.collection.mutable.ListBuffer

abstract class ElementTreeVisitor(val syntacticDocument: SyntacticDocument) {

  private val collector = new ListBuffer[SCPatch]()

  final def collect(value: SCPatch): Unit = {
    collector.+=(value)
  }

  final def result: List[SCPatch] = collector.toList.sortBy(_.startPos)

  private var currentDepth = 0
  final def log( msg: String) = {
    println("  " * currentDepth + msg)
  }

  protected def visitElement(modelElement: ModelElement): Boolean

  final def visit(element: ModelElement): Unit = {

    log("Reached " + element)
    val recurse: Boolean = visitElement(element)
    if(recurse) {
      currentDepth += 1
      element.allChildren.foreach(c => visit(c))
      currentDepth -=1
    }
  }
  lazy val tokens = syntacticDocument.tokens.tokens

  def remove(element: ModelElement, comment: String = ""): Unit = {
    replace(element, "", "remove", comment)
  }

  def replace(element: ModelElement, text: String, actionName: String = "replace", comment: String = ""): Unit = {
    log(s" $actionName(${element.name},'$text')")

    val start = element.annotations.map(a => element.rawStart + a.posOffsetStart - 1).headOption.getOrElse(element.rawStart)
    val candidateBeginToken = tokens.find(t => t.start >= start && t.start <= t.end).head
    val newBeingToken = TokenHelper.whitespaceOrCommentsBefore(candidateBeginToken, syntacticDocument.tokens)
    val newStartPos = newBeingToken.headOption.map(_.start).getOrElse(start)

    collect(SCPatch(newStartPos, element.rawEnd, text, comment))
  }

  def replaceFromFocus(element: ModelElement, text: String, comment: String): Unit = {
    log(s" replaceFromFocus(${element.name},'$text')  ${element.rawFocusStart}->${element.rawEnd}")
    collect(SCPatch(element.rawFocusStart, element.rawEnd, text, comment))
  }

  def addComment(element: ModelElement, msg: String, comment: String = ""): Unit = {
    log(" addComment(" + element.name + ",'" + msg + "')")
    val text = s"/* *** SCALA CLEAN $msg */"
    collect(SCPatch(element.rawStart, element.rawStart, text, comment))
  }


}
