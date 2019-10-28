package org.scalaclean.analysis.plugin

import org.scalaclean.analysis.{StandardExtensionDescriptor, StandardExtensionData}

object ModData extends StandardExtensionDescriptor[ModData]{

  override protected def buildImpl(posOffsetStart: Int, posOffsetEnd: Int, otherParams: String*): ModData = {
    assert (otherParams.length == 2, s"${otherParams.length} - ${otherParams.mkString}")
    new ModData( posOffsetStart,posOffsetEnd, java.lang.Long.parseLong(otherParams(0), 16), otherParams(1))
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

  override def restToCSV: String = s",${flagBit.toHexString},$term"
}


object VisibilityData extends StandardExtensionDescriptor[VisibilityData]{
  override protected def buildImpl(posOffsetStart: Int, posOffsetEnd: Int, otherParams: String*): VisibilityData = {
    assert (otherParams.length == 2)
    new VisibilityData( posOffsetStart,posOffsetEnd, otherParams(0), otherParams(1))
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

  override def restToCSV: String = s",$group,$scope"
}