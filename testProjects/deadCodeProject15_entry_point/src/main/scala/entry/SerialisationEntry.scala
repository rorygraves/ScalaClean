package serentry

object Entry extends App {
  new Used()
}

class Used extends BaseUsed {
  private def unused1 = ???
  def unused2 = ???
  private def readResolve = ???
  private def writeReplace = ???
  private def readObject = ???
  private def writeObject = ???
  private def readObjectNoData = ???
}
class Unused extends BaseUnused{
  private def unused1 = ???
  def unused2 = ???
  private def readResolve = ???
  private def writeReplace = ???
  private def readObject = ???
  private def writeObject = ???
  private def readObjectNoData = ???
}
class Unused2 extends BaseUnused{
  private def unused1 = new Unused2
  def unused2 = new Unused2
  private def readResolve = new Unused2
  private def writeReplace = new Unused2
  private def readObject = new Unused2
  private def writeObject = new Unused2
  private def readObjectNoData = new Unused2
}

class BaseUsed extends Serializable
class BaseUnused

