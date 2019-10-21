package scalaclean.model.impl

import java.nio.file.Path

import scalaclean.model._

import scala.collection.immutable
import scala.reflect.ClassTag

class ProjectSet(projectPropertyPaths: Path *) extends ProjectModel {
  val projects: List[Project] = projectPropertyPaths.toList map { p => Project(p, this)}

  val elements: Map[ElementId, ElementModelImpl] = {
    val (elements, rels: immutable.Seq[BasicRelationshipInfo]) = projects.map(_.read).unzip

    val elementsMap: Map[ElementId, ElementModelImpl] = elements.flatten.toIterator.map (e => e.symbol -> e).toMap
    val modelElements = elements.flatten.toIterator.map (e => e.modelElementId -> e).toMap

    val skipped = elements.flatten.toVector.groupBy (_.symbol).filter{case (k,v) => v.size != 1}
    val skipped2 = elements.flatten.toVector.groupBy (_.modelElementId).filter{case (k,v) => v.size != 1}
    assert (elements.flatten.size == modelElements.size)
//    assert (elementsMap.size == modelElements.size)

    val relsFrom = rels.reduce(_ + _)
    val relsTo = relsFrom.byTo

    relsFrom.complete(elementsMap, modelElements)
    elementsMap.values foreach (_.complete(elementsMap, modelElements, relsFrom = relsFrom, relsTo = relsTo))

    elementsMap
  }

  override def fromSymbolLocal[T <: ModelElement](symbol: ElementId, startPos: Int, endPos: Int)(implicit tpe: ClassTag[T]): T = {
    val x = elements.find {
      case (candidateSymbol: ElementId,em: ValModelImpl) => em.info.startPos == startPos && em.info.endPos == endPos
      case (candidateSymbol,em: VarModelImpl) => em.info.startPos == startPos && em.info.endPos == endPos
      case _ => false
    }

    x.map(_._2) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x: T) => x
      case Some(x) => throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")

    }
  }

  override def fromSymbol[T <: ModelElement](symbol: ElementId)(implicit tpe: ClassTag[T]): T = {
    val targetSymbol = if (symbol.value.startsWith("G:")) symbol  else ElementId("G:" + symbol.value)

    elements.get(targetSymbol) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x: T) => x
      case Some(x) => throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
    }
  }

  override def getElement[T <: ModelElement](symbol: ElementId)(implicit tpe: ClassTag[T]): Option[T] = {
    elements.get(symbol) match {
      case None => None
      case Some(x: T) => Some(x)
      case Some(x) => throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
    }
  }

  override def size: Int = elements.size

  override def allOf[T <: ModelElement : ClassTag]: Iterator[T] = {
    elements.values.iterator collect{
      case x:T => x
    }
  }
}
