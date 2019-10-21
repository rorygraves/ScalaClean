package scalaclean.model.impl

import java.nio.file.Path

import scalaclean.model._

import scala.collection.immutable
import scala.reflect.ClassTag

class ProjectSet(projectPropertyPaths: Path *) extends ProjectModel {
  val projects: List[Project] = projectPropertyPaths.toList map { p => Project(p, this)}

  val elements: Map[ModelSymbol, ElementModelImpl] = {
    val (elements, rels: immutable.Seq[BasicRelationshipInfo]) = projects.map(_.read).unzip

    val elementsMap: Map[ModelSymbol, ElementModelImpl] = elements.flatten.toVector.map (e => e.symbol -> e).toMap

    val relsFrom = rels.reduce(_ + _)
    val relsTo = relsFrom.byTo

    relsFrom.complete(elementsMap)
    elementsMap.values foreach (_.complete(elementsMap, relsFrom, relsTo))

    elementsMap
  }

  override def fromSymbolLocal[T <: ModelElement](symbol: ModelSymbol, startPos: Int, endPos: Int)(implicit tpe: ClassTag[T]): T = {
    val x = elements.find {
      case (candidateSymbol: ModelSymbol,em: ValModelImpl) => em.info.startPos == startPos && em.info.endPos == endPos
      case (candidateSymbol,em: VarModelImpl) => em.info.startPos == startPos && em.info.endPos == endPos
      case _ => false
    }

    x.map(_._2) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x: T) => x
      case Some(x) => throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")

    }
  }

  override def fromSymbol[T <: ModelElement](symbol: ModelSymbol)(implicit tpe: ClassTag[T]): T = {
    val targetSymbol = if (symbol.isGlobal) ModelSymbol("G:" + symbol.value) else {
      elements.keys.find(_.value.contains(symbol.value)).getOrElse {
        throw new IllegalStateException("Unable to match symbol: " + symbol)
      }

    }
    //    elements.get(symbol) match {
    elements.get(targetSymbol) match {
      case None => throw new IllegalArgumentException(s"Unknown symbol $symbol")
      case Some(x: T) => x
      case Some(x) => throw new IllegalArgumentException(s"Unexpected symbol $symbol - found a $x when expecting a ${tpe.runtimeClass}")
    }
  }

  override def getSymbol[T <: ModelElement](symbol: ModelSymbol)(implicit tpe: ClassTag[T]): Option[T] = {
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
