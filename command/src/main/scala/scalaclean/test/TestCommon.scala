package scalaclean.test

import scalaclean.model._

abstract class TestCommon(name: String, model: AllProjectsModel) extends TestBase(name, model) {

  /**
   * Visit the model element.
   * Defaults to calling down to visitInSource if the element is visible in the source code
   */
  def elementInfo(modelElement: ModelElement): String = {
    if (modelElement.existsInSource)
      visitInSource(modelElement)
    else
      ""
  }

  /**
   * Visit a symbol (assuming it is visible in the source code).
   * @param modelElement Target element
   * @return The comment th add about this element.
   */
  def visitInSource(modelElement: ModelElement): String

  def elementAndIdsInTestFormat(test: String, focus: ModelElement, elements: Iterator[ExternalElementReference[_ <: ModelElement]]): String = {
    val strings = elements.map { r =>
      val clOpt = r.elementIfDefined
      val elementId = r.elementId
        clOpt.foreach(cl =>
          assert(cl.modelElementId == elementId, s"cl.modelElementId(${cl.modelElementId}) != elementId($elementId)")
        )
        s"{${clOpt.isDefined}}~${elementId.testOnlyId}"
    }.toList.sorted
    strings match {
      case Nil  => ""
      case refs => refs.mkString(s"$test(${focus.modelElementId.debugValue}) - ", " :: ", "")
    }
  }
  def elementsInTestFormat(test: String, focus: ModelElement, elements: Iterable[ModelElement]): String =
    elementsInTestFormat(test, focus, elements.iterator)
  def elementsInTestFormat(test: String, focus: ModelElement, elements: Iterator[ModelElement]): String = {
    elementIdsTestFormat(test, focus, elements map( _.modelElementId))
  }
  def elementIdsTestFormat(test: String, focus: ModelElement, elements: Iterable[ElementId]): String =
    elementIdsTestFormat(test, focus, elements.iterator)
  def elementIdsTestFormat(test: String, focus: ModelElement, elements: Iterator[ElementId]): String = {
    elements.map(_.testOnlyId).toList.sorted match {
      case Nil  => ""
      case refs => refs.mkString(s"$test(${focus.modelElementId.debugValue}) - ", " :: ", "")
    }
  }

}
