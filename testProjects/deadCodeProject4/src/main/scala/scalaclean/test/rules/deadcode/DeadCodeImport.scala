package scalaclean.test.rules.deadcode.Import

import scala.collection.mutable.HashEntry
import scala.collection.mutable.{HashSet, HashEntry}
import scala.collection.mutable.{HashSet => hs}
import scala.collection.mutable.{HashSet => hs1, HashEntry => he1}

import Outer.{Inner2 => I2}
import Outer.overloaded

import scalaclean.test.rules.deadcode.Import.Outer.{overloaded, Inner2 => I2, NotUsed}

object App1 extends App {
  Outer.Inner
  I2
  println(overloaded(1))
  println()
}
object Outer {
  object Inner
  object Inner2
  object NotUsed

  def overloaded() = 1
  def overloaded(i: Int) = 1
  def overloaded(i: String) = Nil

  def overloadedUnused() = 1
  def overloadedUnused(i: Int) = 1
  def overloadedUnused(i: String) = Nil
}
