package scalaclean.test.deadcode7

object Entry extends App {
  new Entry
}

class Entry extends Parent[Map[P2, P3]] with Another[Map[A2, A3]]

trait Parent[T]
trait Another[T]

class P2
class P3

class A2
class A3

class Unused
