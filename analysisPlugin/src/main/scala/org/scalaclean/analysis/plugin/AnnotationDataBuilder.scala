package org.scalaclean.analysis.plugin

import org.scalaclean.analysis.{AnnotationData, ExtensionData}

import scala.reflect.internal.util.{NoPosition, Position}
import scala.tools.nsc.Global

object AnnotationDataBuilder {
  def buildSimpleAnnotation(g: Global)(annotated: g.Tree, annotation: g.AnnotationInfo): ExtensionData = {
    def print(assoc: g.ClassfileAnnotArg): String = {
      import g._
      assoc match {
        case LiteralAnnotArg(const) => const.value.toString
        case ArrayAnnotArg(args) => args.map(print).mkString(";")
        case NestedAnnotArg(annInfo) =>
          val clazz = annInfo.tpe.typeSymbol.fullName
          //TODO if we really need it
          clean(s"@$clazz(<TODO>)")
        case UnmappableAnnotArg => ???
        case ScalaSigBytes(bytes) => ???
      }
    }

    val clazz = annotation.tpe.typeSymbol.fullName
    val targetPos = annotated.pos
    val pos = if(annotation.pos == NoPosition) Position.range(targetPos.source, 0,0,0) else annotation.pos
    var values = Map.empty[String, String]
    annotation.assocs foreach {
      case (name, assoc) => values = values.updated(name.toString, clean(print(assoc)))
    }
    //TODO cope better with trees - this should cope with @foo(1,2,"hello")
    //TODO cope better with trees - need @foo(x = 1, y = 2, z = "hello")
    for (i <- annotation.scalaArgs.indices) {
      values = values.updated(i.toString, clean(annotation.constantAtIndex(i).map(_.value.toString).getOrElse("<<<TODO>>>")))
    }
    val start = if (pos.isDefined) pos.start - targetPos.start else Int.MinValue
    val end = if (pos.isDefined) pos.end - targetPos.start else Int.MinValue
    AnnotationData(start, end, clazz, values)
  }

  private def clean(s: String): String = s. //
    replace(',', ';').
    replace("\n", "\\n").
    replace("\r", "\\r")

}
