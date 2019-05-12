package scalaclean

class PrivateSuite extends RuleSuite {

  override def rulePath: String = {

    s"${super.rulePath}${sep}test${sep}rules${sep}privatiser${sep}"
  }
}


