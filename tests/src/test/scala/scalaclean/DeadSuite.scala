package scalaclean

import java.io.File

class DeadSuite extends RuleSuite {
  override def rulePath: String = super.rulePath + File.separator + "deadcode"
}
