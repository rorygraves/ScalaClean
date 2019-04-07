package scalaclean

import java.io.File

class MikeSuite extends RuleSuite {
//  override def rulePath: String = super.rulePath + File.separator + "test"+File.separator+"overrides"+File.separator+"internalDirectOverrides"
  override def rulePath: String = super.rulePath + File.separator + "test"+File.separator+"nodes"+File.separator+"nodes"
}
