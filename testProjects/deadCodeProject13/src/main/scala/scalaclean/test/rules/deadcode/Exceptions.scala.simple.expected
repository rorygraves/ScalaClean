package pkg

import scala.util.control.NoStackTrace

object Entry extends App {
  new TestException("Blah")
}

// no changes expected
final case class TestException(message: String) extends RuntimeException(message) with NoStackTrace
