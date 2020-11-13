package scalaclean.rules.privatiser

import scalaclean.model._
import scalaclean.rules.AbstractRule

object SimplePrivatiser extends AbstractRule[SimplePrivatiserCommandLine] {
  override type Rule = SimplePrivatiser

  override def cmdLine                                                          = new SimplePrivatiserCommandLine
  override def apply(options: SimplePrivatiserCommandLine, model: AllProjectsModel) = new Rule(options, model)
}

class SimplePrivatiser(options: SimplePrivatiserCommandLine, model: AllProjectsModel)
    extends AbstractPrivatiser(options, model) {

  override def ruleSpecific(): Unit = {
    model.allOf[ModelElement].foreach {
      case e if e.overriddenByElement().nonEmpty =>
        e.colour = dontChange(s"It's overridden ${e.overriddenByElement().next}")
      case e if e.overridesElementId().nonEmpty =>
        e.colour = dontChange(s"It overrides ${e.overridesElementId().next}")
      case e => e.colour = localLevel(e)
    }
  }

  override def determineAccess(element: ModelElement, myClassLike: ElementId, incomingReferences: Iterable[Refers]): Colour = {
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
      dontChange(s"Its accessed by $refFromOutside")
    else if (refFromCompanion ne null)
      makeChange(Scoped.Private(myClassLike, s"Its private (assessed by companion - $refFromCompanion)"))
    else if (refFromContainer ne null)
      makeChange(Scoped.Private(myClassLike, s"Its private (assessed by enclosing - $refFromContainer)"))
    else
      makeChange(Scoped.Private(myClassLike.dotThis, s"Its private (seems unused)"))
  }

}

class SimplePrivatiserCommandLine extends AbstractPrivatiserCommandLine
