package org.scalaclean.analysis.plugin

import org.scalaclean.analysis.{FlagHelper, StandardExtensionData, StandardExtensionDescriptor}
import scalaclean.model.NewElementId
import scalaclean.model.impl.{NewElementIdImpl, PathNodes}

object ModData extends StandardExtensionDescriptor[ModData] {

  override protected def buildImpl(posOffsetStart: Int, posOffsetEnd: Int, otherParams: String*): ModData = {
    assert(otherParams.length == 1, s"${otherParams.length} - ${otherParams.mkString("[", ",", "]")}")
    new ModData(posOffsetStart, posOffsetEnd, java.lang.Long.parseLong(otherParams(0), 16))
  }
}

/** information about the symbol Mods
  * Note - this only contains mods in the source, and also excludes private and protected @see VisibilityData
  *
  * @param flagBit        a modifier
  * @param posOffsetStart the start offset from the element with this visibility
  * @param posOffsetEnd   the end offset from the element with this visibility
  */
case class ModData(posOffsetStart: Int, posOffsetEnd: Int, flagBit: Long) extends StandardExtensionData {
  override protected def restToString: String = s"flag=${FlagHelper.hexAndString(flagBit)}"

  override def restToCSV: String = s",${flagBit.toHexString}"
}


object VisibilityData extends StandardExtensionDescriptor[VisibilityData] {
  override protected def buildImpl(posOffsetStart: Int, posOffsetEnd: Int, otherParams: String*): VisibilityData = {
    assert(otherParams.length == 2)
    val id = PathNodes.option(otherParams(1))
    new VisibilityData(posOffsetStart, posOffsetEnd, otherParams(0), id)
  }
  val PUBLIC = VisibilityData(0,0,"", None)
}

/** information about the Visibility
  *
  * @param group          "private" or "protected"
  * @param scope          qualifier e.g. x in private[x]
  * @param posOffsetStart the start offset from the element with this visibility
  * @param posOffsetEnd   the end offset from the element with this visibility
  */
case class VisibilityData(posOffsetStart: Int, posOffsetEnd: Int, group: String, scope: Option[NewElementId]) extends StandardExtensionData {
  require(group == "private" || group == "protected" || group == "", group)
  require(!scope.contains(","))

  override def restToCSV: String = s",$group,${scope.map(_.id).getOrElse("")}"
}