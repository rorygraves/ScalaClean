package scalaclean

import java.io.File

class MikeSuite extends RuleSuite {
//  override def rulePath: String = super.rulePath + File.separator + "test"
  //override def rulePath: String = super.rulePath + File.separator + "test"+File.separator+"nodes"+File.separator+"nodes"
  //override def rulePath: String = super.rulePath + File.separator + "test"+File.separator+"overrides"


    override def rulePath: String = super.rulePath + File.separator + "test"+File.separator+"overriddenBy"+File.separator+"internalDirectOverriddenBy"
}
