package scalaclean

import java.io.File

class TestSuite extends RuleSuite {
  override def rulePath: String = super.rulePath + File.separator + "test"
}
