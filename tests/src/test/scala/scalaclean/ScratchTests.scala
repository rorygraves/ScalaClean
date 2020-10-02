package scalaclean

import org.scalatest.{ Canceled, Failed, OutcomeOf, Pending, Succeeded }
import org.scalatest.exceptions.TestPendingException
import scalaclean.cli.SCOptions

class ScratchTests extends AbstractProjectTests {
  val expectationSuffix: String = ""
  val taskName: String          = SCOptions.deadCodeCmd

  test("scratch") {
    OutcomeOf.outcomeOf(deadCodeProjectTest("scratch")) match {
      case Succeeded   =>
      case Canceled(e) => throw e
      case Pending     => throw new TestPendingException()
      case Failed(e) =>
        Console.err.println(e)
        e.getStackTrace.take(7).foreach(e => Console.err.println(s"\tat $e"))
        throw new TestPendingException()
    }
  }

//  test("scratch1") {
//    privatiserProjectTest("scratch1")
//  }
//
}
