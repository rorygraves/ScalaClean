package scalaclean

class MikeSuite extends RuleSuite {
  override def rulePath: String = s"${super.rulePath}${sep}test${sep}references${sep}internalIncomingReferences"
}
