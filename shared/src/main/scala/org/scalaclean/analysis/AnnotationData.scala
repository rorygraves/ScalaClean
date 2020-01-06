package org.scalaclean.analysis

object AnnotationData extends StandardExtensionDescriptor[AnnotationData] {

  override protected def buildImpl(posOffsetStart: Int, posOffsetEnd: Int, otherParams: String*): AnnotationData = {
    val map: Map[String, String] = otherParams.toList.drop(1).grouped(2).collect {
      case k :: v :: Nil => k -> v
      case x :: Nil =>
        println(s"***** $x")
        x.toString -> "UNKNOWN"
    }.toMap
    new AnnotationData(posOffsetStart, posOffsetEnd, otherParams(0), map)
  }
}

/** information about an annotation.
  *
  * @param fqName         the name of the annotationClass
  * @param values         arguments and values passed to the annotation
  * @param posOffsetStart the start offset from the element with this visibility
  * @param posOffsetEnd   the end offset from the element with this visibility
  */
case class AnnotationData(posOffsetStart: Int, posOffsetEnd: Int, fqName: String, values: Map[String, String]) extends StandardExtensionData {

  import scala.collection.SortedMap

  override def restToCSV: String = {
    val csv = if (values.isEmpty) "" else values.toList.sortBy(_._1).map { e => s"${e._1},${e._2}" }.mkString(",", ",", "")
    s",${fqName}${csv}"
  }

  override protected def restToString: String = s"$fqName,${SortedMap.empty[String, String] ++ values}"

  require(!fqName.contains(","), fqName)
  values.foreach {
    case (k, v) =>
      require(!k.contains(","), s"key of $k")
      require(!v.contains(","), s"value of $v")
  }
}