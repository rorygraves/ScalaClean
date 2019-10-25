package org.scalaclean.analysis

object AnnotationData extends ExtensionDescriptor[AnnotationData] {

  override protected def build(s: String): AnnotationData = {
    val params = s.split(",").map (_.intern)
    val map: Map[String, String] = params.toList.drop(3).grouped(2).map {
      case k :: v :: Nil => k -> v
      case _ => ???
    }.toMap
    new AnnotationData( params(0).toInt,params(1).toInt, params(2), map)
  }

}
/** information about an annotation.
  *
  * @param fqName th name of the annotationClass
  * @param values qualifier e.g. x in private[x]
  * @param posOffsetStart the start offset from the element with this visibility
  * @param posOffsetEnd the end offset from the element with this visibility
  */
case class AnnotationData (posOffsetStart: Int, posOffsetEnd: Int, fqName: String, values: Map[String,String]) extends StandardExtensionData{
  override def toCsv: String = {
    val csv = values.toList.sortBy(_._1).map{e => s"${e._1},${e._2}"}.mkString(",", ",", "")
    s"${posOffsetStart},${posOffsetStart},${fqName},${csv}"
  }
  require(!fqName.contains(","), fqName)
  values.foreach {
    case (k, v) =>
      require(!k.contains(","), s"key of $k")
      require(!v.contains(","), s"value of $v")
  }
}