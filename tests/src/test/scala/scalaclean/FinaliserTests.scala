package scalaclean

import org.junit.Test

class FinaliserTests extends AbstractProjectTests {

  @Test def finaliser1: Unit = {
    finaliserProjectTest("finaliserProject1")
  }

}
