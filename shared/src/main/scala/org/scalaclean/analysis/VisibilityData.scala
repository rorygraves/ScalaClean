package org.scalaclean.analysis

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