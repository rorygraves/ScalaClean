package scalaclean

import java.io.File

class DeadSuite extends RuleSuite {
  override def rulePath: String = s"${super.rulePath}${sep}test${sep}rules${sep}deadcode"
}
