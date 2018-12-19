package scalaclean.model

class SCModel {
  def printStructure(): Unit = {
    // TODO
    classes.foreach { case (name, value) =>
      println(s"Class: $name - outer = ${value.outerCls}")

    }
  }

  var classes: Map[String, SCClass] = Map.empty
  def getOrCreateClass(fullName: String): SCClass = {
    classes.get(fullName) match {
      case Some(c) => c
      case None =>
        val c = new SCClass(fullName)
        classes += fullName -> c
        c
    }
  }
}

class SCClass(fullName: String) {
  var outerCls: Option[SCClass] = None
  def setOuter(outerClass: Option[SCClass]) : Unit = {
    this.outerCls = outerClass
  }

  // TODO method list
  // TODO class reference list (incoming/outgoing)

}
