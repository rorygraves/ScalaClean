package scalaclean.rules.deadcode

import scalaclean.cli.RunOptions
import scalaclean.model._

class SimpleDeadCode(model: ProjectModel, options: RunOptions) extends DeadCodeRemover(model, options) {

  override val name: String = "SimpleDeadCodeRemover"

  override def markIndirectReferences = false

  override def markRhs(element: ModelElement, purpose: Purpose, path: List[ModelElement], comment: String): Unit = ()

  //TODO plugin for tweaks
  //e.modelElementId.id.contains("_:=")
  protected def extendedUsedDirectly(e: ModelElement): Boolean = false

  protected def isUsedDirectly(e: ModelElement): Boolean = {
    e.incomingReferences.nonEmpty ||
    extendedUsedDirectly(e) ||
    e.allTransitiveOverrides.nonEmpty ||
    e.annotations.exists(ad => otherAnnotationBasedEntryPoints.contains(ad.fqName))
  }

  override def runRule(): Unit = {
    super.runRule()

    model
      .allOf[ModelElement]
      .foreach(e =>
        if (isUsedDirectly(e))
          markUsed(e, markEnclosing = false, Main, e :: Nil, "")
      )

    //Not sure why
    model.allOf[ValModel].foreach { e =>
      if (e.isLazy && e.internalTransitiveOverrides.nonEmpty)
        markUsed(e, markEnclosing = false, Main, e :: Nil, "lazy")
    }
  }

}
