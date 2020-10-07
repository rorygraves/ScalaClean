package scalaclean.rules.privatiser

import scalaclean.model._
import scalaclean.rules.AbstractRule

object SimplePrivatiser extends AbstractRule[SimplePrivatiserCommandLine] {
  override type Rule = SimplePrivatiser

  override def cmdLine                                                          = new SimplePrivatiserCommandLine
  override def apply(options: SimplePrivatiserCommandLine, model: ProjectModel) = new Rule(options, model)
}

class SimplePrivatiser(options: SimplePrivatiserCommandLine, model: ProjectModel)
    extends AbstractPrivatiser(options, model) {


  def isOverridden(e: ModelElement): Boolean = {
    e.internalTransitiveOverriddenBy.nonEmpty
  }

  def isOverrides(e: ModelElement): Boolean = {
    e.allTransitiveOverrides.nonEmpty
  }

  override def ruleSpecific(): Unit = {
    model.allOf[ModelElement].foreach {
      case e if isOverridden(e) =>
        e.colour = NoChange(s"It's overridden ${e.internalTransitiveOverriddenBy.head}")
      case e if isOverrides(e) =>
        e.colour = NoChange(s"It overrides ${e.allTransitiveOverrides.head}")
      case e => e.colour = localLevel(e)
    }
  }

  override def determineAccess(element: ModelElement, myClassLike: ElementId, incomingReferences: Iterable[Refers]): PrivatiserLevel = {
    val myId = element.modelElementId

    var refFromContainer: ModelElement = null
    var refFromCompanion: ModelElement = null
    var refFromOutside: ModelElement   = null
    incomingReferences.foreach { ref =>
      val from = ref.fromElement
      if (from.modelElementId.equalsOrHasParent(myId)) {
        if (debug)
          println("internal reference can be ignored")
      } else if (from.modelElementId.equalsOrHasParent(myClassLike))
        refFromContainer = from
      else if (from.modelElementId.equalsOrHasParentScope(myClassLike))
        refFromCompanion = from
      else
        refFromOutside = from
    }
    if (refFromOutside ne null)
      NoChange(s"Its accessed by $refFromOutside")
    else if (refFromCompanion ne null)
      Scoped.Private(myClassLike, s"Its private (assessed by companion - $refFromCompanion)")
    else if (refFromContainer ne null)
      Scoped.Private(myClassLike, s"Its private (assessed by enclosing - $refFromContainer)")
    else
      Scoped.Private(myClassLike.dotThis, s"Its private (seems unused)")
  }

}

class SimplePrivatiserCommandLine extends AbstractPrivatiserCommandLine
