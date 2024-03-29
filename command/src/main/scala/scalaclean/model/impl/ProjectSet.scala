package scalaclean.model.impl

import java.nio.file.Path

import scalaclean.model._

import scala.collection.immutable
import scala.reflect.{ClassTag, classTag}

class ProjectSet(projectPropertyPaths: Path*) extends AllProjectsModel {
  override val projects: List[ProjectImpl] = projectPropertyPaths.toList.map(p => ProjectImpl(p, this))

  val elements: Map[ElementId, ElementModelImpl] = {
    val (elements, rels: immutable.Seq[BasicRelationshipInfo]) = projects.map(_.read).unzip

    val modelElements = elements.flatten.toIterator.map(e => e.modelElementId -> e).toMap

    if (elements.flatten.size != modelElements.size) {

      val duplicates = elements.flatten.groupBy(_.modelElementId).filter { case (k, v) => v.size != 1 }

      println("Duplicate SYMBOLS ")
      duplicates.foreach { case (s, values) =>
        println(s"  $s")
        values.foreach(v => println(s"    $v"))
      }

      throw new IllegalStateException("Duplicate elements found")
    }
 {
   import scala.concurrent.ExecutionContext.Implicits._
   import scala.concurrent.{Await, Future}
   import scala.concurrent.duration.Duration
    val basicRels = rels.reduce(_ + _)
    val relsFromF = Future(basicRels.sortValues)
    val relsToF   = Future(basicRels.byTo.sortValues)

    basicRels.complete(modelElements)
    val relsFrom = Await.result(relsFromF, Duration.Inf)
    val relsTo   = Await.result(relsToF, Duration.Inf)

   //we cant complete in parallel - there are linkages
    modelElements.values.foreach(_.complete(ElementIds, modelElements, relsFrom = relsFrom, relsTo = relsTo))
 }

    ModelReader.finished()
    modelElements
  }

  override def element[T <: ModelElement: ClassTag: NotNothing](id: ElementId): T = {

    elements.get(id) match {
      case None       => throw new IllegalArgumentException(s"Unknown element $id")
      case Some(x: T) => x
      case Some(x) =>
        throw new IllegalArgumentException(s"Unexpected element $id - found a $x when expecting a ${implicitly[ClassTag[T]].runtimeClass}")
    }
  }

  override def getElement[T <: ModelElement: ClassTag: NotNothing](id: ElementId): Option[T] = {
    elements.get(id) match {
      case None       => None
      case Some(x: T) => Some(x)
      case Some(x) =>
        throw new IllegalArgumentException(s"Unexpected element $id - found a $x when expecting a ${implicitly[ClassTag[T]].runtimeClass}")
    }
  }

  override def size: Int = elements.size

  private val allCache = new ClassValue[Iterable[ModelElement]] {
    override def computeValue(tpe: Class[_]): Iterable[ModelElement] = {
      elements.values.iterator.filter{ e => tpe.isInstance(e)}.toArray(ClassTag(tpe))
    }
  }

  override def allOf[T <: ModelElement: ClassTag : NotNothing]: Iterator[T] = {
    allCache.get(implicitly[ClassTag[T]].runtimeClass).asInstanceOf[Iterable[T]].iterator
  }

}
