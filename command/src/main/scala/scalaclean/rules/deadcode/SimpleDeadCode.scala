package scalaclean.rules.deadcode

import scalaclean.model.{Overrides, _}

class SimpleDeadCode(model: ProjectModel, debug: Boolean) extends DeadCodeRemover(model, debug) {

  override val name: String = "SimpleDeadCodeRemover"

  // TODO should it override and use 'markEnclosing?
  def markUsed(element: ModelElement, purpose: Purpose, path: List[ModelElement], comment: String): Unit = {
    val current = element.colour
    if(!current.hasPurpose(purpose)) {
      println(s"mark $element as used for $purpose due to ${path.mkString("->")} $comment - elementID: ${element.modelElementId}")
      element.colour = current.withPurpose(purpose)

      //enclosing
      element.enclosing foreach {
        enclosed => markUsed(enclosed, purpose, element :: path, s"$comment - enclosing")
      }

      //overrides
      element.overrides() flatMap (_.toElement) foreach {
        overriddenByElement => markUsed(overriddenByElement, purpose, element :: path, s"$comment - overrides")
      }

      //If i am used mark everything overriding me as used
      element.overriddenBy() map (_.fromElement) foreach {
        overridesElement => markUsed(overridesElement, purpose, element :: path, s"$comment - overridden by")
      }

      if(element.isInstanceOf[ObjectModel]) {
        element.methods.foreach {m => if(m.modelElementId.id.contains("apply(")) markUsed(m, purpose, element :: path,s"$comment - apply method of used object")}
      }

      // all the elements that this refers to
      element.internalOutgoingReferences foreach {
        case (ref, _) => markUsed(ref, purpose, element :: path, s"$comment, - internalOutgoingReferences")
      }

      if(element.isInstanceOf[TraitModel]) {
        element.fields.foreach {
          fieldsInTrait => markUsed(fieldsInTrait, purpose, element :: path, s"$comment - inside a used trait")
        }
      }

    }
  }

  def isUsed(e: ModelElement): Boolean = {
    val usedAnnotations = Set[String](
      "org.junit.Test"
    )
    e.incomingReferences.nonEmpty || e.modelElementId.id.contains("_:=") || e.allTransitiveOverrides.nonEmpty || e.annotations.exists(ad => usedAnnotations.contains(ad.fqName))
  }

  override def runRule(): Unit = {
    model.allOf[ModelElement].foreach {
      e => if(isUsed(e)) markUsed(e, Main, e :: Nil, "")
    }

    allMainEntryPoints.foreach {
      e => markUsed(e, Main, e :: Nil, "")
    }

    allTestEntryPoints.foreach {
      e => markUsed(e, Main, e :: Nil, "")
    }

    model.allOf[ValModel].foreach { e => if(e.isLazy && e.overrides().nonEmpty) markUsed(e, Main, e :: Nil,"lazy")}
  }
}
