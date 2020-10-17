package scalaclean.rules.deadcode

import scalaclean.model._
import scalaclean.rules.AbstractRule

import scala.collection.mutable

object SimpleDeadCodeRemover extends AbstractRule[SimpleDeadCodeCommandLine] {
  override type Rule = SimpleDeadCodeRemover

  override def cmdLine                                                        = new SimpleDeadCodeCommandLine
  override def apply(options: SimpleDeadCodeCommandLine, model: ProjectModel) = new Rule(options, model)

}

class SimpleDeadCodeRemover(override val options: SimpleDeadCodeCommandLine, override val model: ProjectModel)
    extends AbstractDeadCodeRemover[SimpleDeadCodeCommandLine] {

  override def markIndirectReferences = false

  override def markRhs(element: ModelElement, purpose: Purpose, path: List[ModelElement], comment: String): Unit = ()

  //TODO plugin for tweaks
  //e.modelElementId.id.contains("_:=")
  protected def extendedUsedDirectly(e: ModelElement): Boolean = false

  def incomingReferences(fieldModel: FieldModel): Iterator[ModelElement] = {
    //all of the direct references + all of the references in from the getters/setters that are compiler generated ( not in source)
    //and filter out the internal accesses from the accessors
    fieldModel.accessors.iterator.filter {
      //lazy vals have an accessor which has the same start/end as the val
      //TODO fix this in the model
      accessor =>
        !accessor.existsInSource || (
          fieldModel.existsInSource &&
            fieldModel.rawStart == accessor.rawStart &&
            fieldModel.rawEnd == accessor.rawEnd)
    }
  }

  def simpleMarkUsed(subject: ModelElement, accessor: ElementId, comment: String): Unit = {
    if (debug) {
      println(s"[SimpleDeadCode] mark ${subject.modelElementId} as used due to ${accessor} $comment")
    }
    moreMarked = true
    subject.colour = subject.colour.withPurpose(Main)
  }
  var moreMarked = false

  protected def checkUsedDirectly(element: ModelElement): Unit = {
    def encloses(e: ModelElement): Boolean = e.sourceFileName == element.sourceFileName &&
      e.rawStart >= element.rawStart &&
      e.rawEnd <= element.rawEnd

    val tried = new mutable.HashSet[ModelElement]
    val todo  = new mutable.HashSet[ModelElement]
    def oneLevel(thisElement: ModelElement) {
      val extra = thisElement match {
        case fieldModel: FieldModel =>
          incomingReferences(fieldModel)
        case fieldsModel: FieldsModel =>
          fieldsModel.fieldsInDeclaration.flatMap(incomingReferences)
        case _ => Nil
      }
      val refs = thisElement.internalIncomingReferences.map(_._1).iterator ++ extra
      refs.filterNot(tried).foreach {
        case e1 if e1.existsInSource && !encloses(e1) =>
          simpleMarkUsed(element, e1.modelElementId, "references")
          return
        case e1 =>
          todo += e1
      }
    }
    todo += element
    while (element.colour.isUnused && !todo.isEmpty) {
      val next = todo.head
      todo -= next
      tried += next
      oneLevel(next)
    }

  }

  override def runRule(): Unit = {
    super.runRule()

    //see what we can see is used
    model
      .allOf[ModelElement]
      .foreach(e =>
        if (e.colour.isUnused)// we need to mark thing used that are not in source
          checkUsedDirectly(e)
      )

    // classes are used in there is is extended by something in use
    model
      .allOf[ClassLike]
      .foreach(e =>
        if (e.colour.isUnused && e.existsInSource) {
          e.extendedByClassLike(false, (_, cls) => !cls.colour.isUnused).take(1).foreach{
            ele => simpleMarkUsed(e, ele.modelElementId, "extendedBy")
          }
        }
      )
    // methods which overiddes something  external ( so we but implement, as we cant remove)
    model
      .allOf[MethodModel]
      .foreach(e =>
        if (e.colour.isUnused && e.existsInSource) {
          e.allTransitiveOverrides.iterator.collectFirst{
            case (None, id) => id
          }.foreach{ id =>
            simpleMarkUsed(e, id, s"overrides external")
          }
        }
      )

    do {
      //we need to repeat here because you can multiply inherit so we want the trees to be consistent
      //e.g.
      //trait A {def foo = 1 }
      //trait B {def foo = 1 }
      //class Both1 extends A with B
      //class Both2 extends A
      //and a reference to new Both2().foo
      //for the simple rule we should keep A.foo and as B overlaps, keep B.foo

      moreMarked = false

      // methods which have overrides in use
      model
        .allOf[MethodModel]
        .foreach(e =>
          if (e.colour.isUnused) {
            e.internalTransitiveOverriddenBy.iterator.filterNot(_.colour.isUnused).take(1).foreach {
              ele => simpleMarkUsed(e, ele.modelElementId, "overridden by used")
            }
          }
        )
      // methods which overiddes something in use, or something external ( so we but implement, as we cant remove)
      model
        .allOf[MethodModel]
        .foreach(e =>
          if (e.colour.isUnused) {
            e.internalTransitiveOverrides.iterator.collectFirst {
              case ele if !ele.colour.isUnused => ele
            }.foreach {
              ele => simpleMarkUsed(e, ele.modelElementId, s"overrides internal ")
            }
          }
        )
    } while (moreMarked)

  }

}

class SimpleDeadCodeCommandLine extends AbstractDeadCodeCommandLine
