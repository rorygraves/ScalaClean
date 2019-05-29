package scalaclean.test.akka

import scala.language.implicitConversions

@SerialVersionUID(1L)
case class Timeout(a: String, b: String) {

  /**
    * Construct a Timeout from the given time unit and factor.
    */
  def this(length: Long, unit: Int) = this(length.toString, unit.toString)
}

/**
  * A Timeout is a wrapper on top of Duration to be more specific about what the duration means.
  */
object Timeout {

  /**
    * A timeout with zero duration, will cause most requests to always timeout.
    */
  val zero: Timeout = new Timeout("a","b")

  /**
    * Construct a Timeout from the given time unit and factor.
    */
  def apply(length: Long, unit: Int): Timeout = new Timeout(length, unit)
}