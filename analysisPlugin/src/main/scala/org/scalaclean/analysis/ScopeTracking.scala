package org.scalaclean.analysis

trait ScopeTracking {

  var scopeStack: List[ModelSymbol] = Nil

  val debug: Boolean

  def withinScope[T](mSymbol: ModelSymbol)(fn: => T): Unit = {
    if(debug)
      println("  " * scopeStack.size + "   Entering " + mSymbol)
    scopeStack = mSymbol :: scopeStack
    fn
    scopeStack = scopeStack.tail
    if(debug)
      println("  " * scopeStack.size +"   Exiting " + mSymbol)
  }

  def parentScope: ModelSymbol = scopeStack.head
  def parentGlobalScope: ModelSymbol = scopeStack.find(_.isGlobal).get


}
