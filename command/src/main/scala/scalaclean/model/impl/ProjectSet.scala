package scalaclean.model.impl

import java.nio.file.Path

import scalaclean.model._

import scala.collection.immutable
import scala.reflect.ClassTag

class ProjectSet(projectPropertyPaths: Path*) extends ProjectModel {
  val projects: List[Project] = projectPropertyPaths.toList map { p => Project(p, this) }

  val (legacyElements: Map[LegacyElementId, Seq[ElementModelImpl]], elements: Map[ElementId, ElementModelImpl]) = {
    val (elements, rels: immutable.Seq[BasicRelationshipInfo]) = projects.map(_.read).unzip

    val elementsMap: Map[LegacyElementId, Seq[ElementModelImpl]] = elements.flatten.groupBy(_.legacySymbol)
    val modelElements = elements.flatten.toIterator.map(e => e.elementId -> e).toMap

    def duplicates = {
      val skipped = elementsMap.filter { case (k, v) => v.size != 1 }
      val skipped2 = elements.flatten.groupBy(_.elementId).filter { case (k, v) => v.size != 1 }

      (skipped, skipped2)

    }

    if (elements.flatten.size != modelElements.size) {

      val (orig, newTokens) = duplicates

      println("Duplicate OLD SYMBOLS ")
      orig.foreach { case (s, values) =>
        println(s"  $s")
      }

      println("Duplicate NEW SYMBOLS ")
      newTokens.foreach { case (s, values) =>
        println(s"  $s")
        values.foreach { v =>
          println(s"    $v")
        }
      }

      throw new IllegalStateException("Duplicate elements found")
    }

    val relsFrom = rels.reduce(_ + _)
    val relsTo = relsFrom.byTo

    relsFrom.complete(modelElements)
    modelElements.values foreach (_.complete(modelElements, relsFrom = relsFrom, relsTo = relsTo))

    ModelReader.finished()
    (elementsMap, modelElements)
  }

  override def legacySymbol[T <: ModelElement](symbol: LegacyElementId)(implicit tpe: ClassTag[T]): T = {
    val targetSymbol = if (symbol.value.startsWith("G:")) symbol else LegacyElementId("G:" + symbol.value)

    legacyElements.get(targetSymbol) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x) => x.collectFirst {case x: T => x} getOrElse (
        throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}"))
    }
  }

  override def getLegacySymbol[T <: ModelElement](symbol: LegacyElementId)(implicit tpe: ClassTag[T]): Option[T] = {
    legacyElements.get(symbol) match {
      case None => None
      case Some(x) => x.collectFirst {case x: T => x} orElse (
        throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}"))
    }
  }

  override def element[T <: ModelElement](id: ElementId)(implicit tpe: ClassTag[T]): T = {

    elements.get(id) match {
      case None => throw new IllegalArgumentException(s"Unknown element $id")
      case Some(x: T) => x
      case Some(x) => throw new IllegalArgumentException(s"Unexpected element $id - found a $x when expecting a ${tpe.runtimeClass}")
    }
  }

  override def getElement[T <: ModelElement](id: ElementId)(implicit tpe: ClassTag[T]): Option[T] = {
    elements.get(id) match {
      case None => None
      case Some(x: T) => Some(x)
      case Some(x) => throw new IllegalArgumentException(s"Unexpected element $id - found a $x when expecting a ${tpe.runtimeClass}")
    }
  }

  override def size: Int = elements.size

  override def allOf[T <: ModelElement : ClassTag]: Iterator[T] = {
    elements.values.iterator collect {
      case x: T => x
    }
  }
}
