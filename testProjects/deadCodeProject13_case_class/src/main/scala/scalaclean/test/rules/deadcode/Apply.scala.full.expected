package caseclass.apply

object Entry extends App {
  CaseWithApply("Blah")
  new CaseWithNew("Blah")
  new ExplicitNew1("Blah")
  ExplicitApply1("Blah")
  new ExplicitNew2("Blah")
  ExplicitApply2("Blah")

  //so we don't try to removeBase.x
  new Base("").s
}

class Base(val s:String)

// no changes expected
case class CaseWithApply(x: String) extends Base(x)
case class CaseWithNew(x: String) extends Base(x)

object ExplicitApply1 {
  def apply(a: String) = new ExplicitApply1(a)
}
class ExplicitApply1(x: String) extends Base(x)
class ExplicitNew1(x: String) extends Base(x)

object ExplicitApply2 {
  def apply(a: String) = new ExplicitApply2(a)
}
class ExplicitApply2(val x: String) extends Base(x)
class ExplicitNew2(val x: String) extends Base(x)
