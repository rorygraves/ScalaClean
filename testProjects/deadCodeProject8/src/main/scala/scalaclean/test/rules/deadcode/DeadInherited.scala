package scalaclean.test.rules.deadcode

object Entry1 extends App {
  (new Entry1).foo
}

class Entry1 extends Parent1 with Another1

trait Parent1 {
  def foo         = 1
  def unused: Int = 1
}

trait Another1 {
  def foo: Int
  def unused2: Int = 1
}

object Entry2 extends App {
  (new Entry2).foo
}

class Entry2 extends Parent2 with Another2

class Parent2 {
  def foo         = 1
  def unused: Int = 1
}

trait Another2 {
  def foo: Int
  def unused2: Int = 1
}

object Entry3 extends App {
  (new Entry3).foo
}

class Entry3 extends Parent3 with Another3 {
  override def foo = 7
}

class Parent3 {
  def foo         = 1
  def unused: Int = 1
}

trait Another3 {
  def foo: Int
  def unused2: Int = 1
}
