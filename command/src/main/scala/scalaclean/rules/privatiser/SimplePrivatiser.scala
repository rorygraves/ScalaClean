package scalaclean.rules.privatiser

import scalaclean.cli.RunOptions
import scalaclean.model._

class SimplePrivatiser(model: ProjectModel, options: RunOptions) extends Privatiser(model, options) {

  override val name = "SimplePrivatiser"

  def isPublic(e: ModelElement): Boolean = {
    val usedAnnotations: Set[String]= Set(
      // TODO need some sensible defaults here
    )
    // e.symbol.symbol.value.endsWith("_:=) ||
    e.annotations.exists(ad =>
      usedAnnotations.contains(ad.fqName))
  }

  def isOverridden(e: ModelElement): Boolean = {
    e.internalTransitiveOverriddenBy.nonEmpty
  }

  def isOverrides(e: ModelElement): Boolean = {
    e.allTransitiveOverrides.nonEmpty
  }

  override def runRule(): Unit = {
    allApp foreach {
      e => e.mark = NoChange("It's an app")
    }

    allJunitTest foreach {
      e => e.mark = NoChange("It's a test")
    }

    model.allOf[ModelElement].foreach {
      case e: SourceModel =>
        e.colour = NoChange("source")
      case e if isPublic(e) =>
        e.colour = NoChange("rule/annotation")
      case e if e.isAbstract =>
        e.colour = NoChange("It's abstract")
      case e if isOverridden(e) =>
        e.colour = NoChange(s"It's overridden ${e.internalTransitiveOverriddenBy.head}")
      case e if isOverrides(e) =>
        e.colour = NoChange(s"It overrides ${e.allTransitiveOverrides.head}")
      case e => e.colour = localLevel(e)
    }
    model.allOf[ModelElement].toList.sortBy(_.infoPosSorted).foreach(ele =>
      println(s"${ele}  colour: ${ele.colour}"))
  }

  override def localLevel(element: ModelElement): PrivatiserLevel = {
    element match {
      case _ if element.colour != Undefined => element.colour
      case _: SourceModel => NoChange("This is a source element.")
      case _ if element.enclosing.exists(_.isInstanceOf[SourceModel]) => NoChange("This is a top level element")
      case _ if element.enclosing.size == 1 =>
        val parent = element.enclosing.head
        val (file, start, end) = parent.infoPosSorted
        // companion object situation
        val companion = element.incomingReferences.filter {
          companion =>
            val refersFromEnclosing = element.incomingReferences.map(_.fromElement.enclosing.head).toSeq.distinct
            val (fileFrom, startFrom, endFrom) = companion.fromElement.infoPosSorted
           // TODO MIKE - referce to old symbols
           // fileFrom == file && startFrom != start && endFrom != end && refersFromEnclosing.size == 1 && companion.fromElement.enclosing.head.symbol == element.enclosing.head.symbol
            false
        }
        companion match {
          case c if c.nonEmpty => Scoped.Private(parent.modelElementId, "It is a companion situation")
          case _ =>
            // refer & override
            val referred = element.incomingReferences.find {
              refers =>
                val (fileFrom, startFrom, endFrom) = refers.fromElement.infoPosSorted
                fileFrom != file || startFrom < start || endFrom > end
            }
            referred match {
              //referred
              case Some(r) => NoChange(s"Its referred by other ${r.fromElement}")
              //overrides
              case None =>
                Scoped.Private(parent.modelElementId, "Its private")
            }
        }
    }
  }
}
