package org.scalaclean.analysis.plugin

import org.scalaclean.analysis.{ExtensionDescriptor, StandardExtensionData}

object ModData extends ExtensionDescriptor[ModData]{
  override protected def build(s: String): ModData = {
    val params = s.split(",")
    assert (params.length <= 4, s"${params.length} - $s : ${params.mkString}")
    assert (params.length >= 3, s"${params.length} - $s : ${params.mkString}")
    new ModData( params(0).toInt,params(1).toInt, java.lang.Long.parseLong(params(2), 16), if (params.length == 4) params(3).intern else "")
  }
}

/** information about the symbol Mods
  * Note - this only contains mods in the source, and also excludes private and protected @see VisibilityData
  *
  * @param term a keyword in the source, e.g. lazy, case
  * @param posOffsetStart the start offset from the element with this visibility
  * @param posOffsetEnd the end offset from the element with this visibility
  */
case class ModData (posOffsetStart: Int, posOffsetEnd: Int, flagBit: Long, term: String) extends StandardExtensionData{
  require(!term.contains(","))

  override def toCsv: String = s"$posOffsetStart,$posOffsetEnd,${flagBit.toHexString},$term"
}


object VisibilityData extends ExtensionDescriptor[VisibilityData]{
  override protected def build(s: String): VisibilityData = {
    val params = s.split(",")
    assert (params.length == 4)
    new VisibilityData( params(0).toInt,params(1).toInt, params(2).intern, params(3).intern)
  }
}

/** information about the Visibility
  *
  * @param group "private" or "protected"
  * @param scope qualifier e.g. x in private[x]
  * @param posOffsetStart the start offset from the element with this visibility
  * @param posOffsetEnd the end offset from the element with this visibility
  */
case class VisibilityData ( posOffsetStart: Int, posOffsetEnd: Int, group: String, scope: String) extends StandardExtensionData{
  require(group == "private" || group == "protected" || group == "", group)
  require(!scope.contains(","))

}