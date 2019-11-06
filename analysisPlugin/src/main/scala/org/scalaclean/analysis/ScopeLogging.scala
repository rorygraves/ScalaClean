package org.scalaclean.analysis

trait ScopeLogging {
  val debug: Boolean

  def scopeLog(msg: => String): Unit
}
