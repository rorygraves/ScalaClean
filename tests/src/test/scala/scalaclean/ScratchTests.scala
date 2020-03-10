package scalaclean

import java.nio.file.NoSuchFileException

import org.scalatest.exceptions.TestPendingException

class ScratchTests extends AbstractProjectTests {
  test("scratch") {
    try deadCodeProjectTest("scratch") catch {
      case e: NoSuchFileException => throw new TestPendingException().initCause(e)
    }
  }

//  test("scratch1") {
//    privatiserProjectTest("scratch1")
//  }
//
}
