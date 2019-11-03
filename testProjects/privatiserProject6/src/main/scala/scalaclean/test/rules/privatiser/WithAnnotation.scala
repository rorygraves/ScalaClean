package x1.scalaclean.test.rules.privatiser.withannotation

object Entry extends App {
  (new Entry).foo
}
@deprecated
class Entry {
  @deprecated
  def foo:Int = foo+1
}