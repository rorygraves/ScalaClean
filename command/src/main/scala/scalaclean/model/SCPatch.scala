package scalaclean.model

final case class SCPatch(startPos: Int, endPos: Int, replacementText: String, comment:String = "")
