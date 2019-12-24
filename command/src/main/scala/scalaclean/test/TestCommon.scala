package scalaclean.test

import scalaclean.model._

abstract class TestCommon(name: String, model: ProjectModel) extends TestBase(name, model) {
  /** Visit the model element.
   * Defaults to calling down to visitInSource if the element is visible in the source code */
  def elementInfo(modelElement: ModelElement): String = {
    if(modelElement.existsInSource)
      visitInSource(modelElement)
    else
      ""
  }

  /**
   * Visit a symbol (assuming it is visible in the source coee.
   * @param modelElement Target element
   * @return The comment th add about this element.
   */
  def visitInSource(modelElement: ModelElement): String

  def debugValues(elements: Seq[ModelElement]): Seq[String] = {
    elements map (_.modelElementId.debugValue)
  }
}
