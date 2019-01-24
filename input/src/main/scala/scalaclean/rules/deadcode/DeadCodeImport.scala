/*
rules = [ Analysis , ScalaCleanDeadCodeRemover ]

*/
package scalaclean.rules.deadcode.Import

import scala.collection.mutable.HashEntry
import scala.collection.mutable.{HashSet, HashEntry}
import scala.collection.mutable.{HashSet => hs}
import scala.collection.mutable.{HashSet => hs1, HashEntry => he1}

import Outer.{Inner2 => I2}
import Outer.NotUsed
import Outer.{NotUsed}
import Outer.{NotUsed, Inner}
import Outer.{Inner, NotUsed}
import Outer.{Inner, NotUsed => nu}

import Outer.overloaded
import Outer.overloadedUnused

object App1 extends App {
  Outer.Inner
  I2
  println()
}
object Outer {
  object Inner
  object Inner2
  object NotUsed

  println(overloaded(1))

  def overloaded() = 1
  def overloaded(i: Int) = 1
  def overloaded(i: String) = Nil

  def overloadedUnused() = 1
  def overloadedUnused(i: Int) = 1
  def overloadedUnused(i: String) = Nil
}
