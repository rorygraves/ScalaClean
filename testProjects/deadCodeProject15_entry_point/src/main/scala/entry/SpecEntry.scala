package specentry

import org.scalatest.Suite
import org.scalatest.funsuite.AnyFunSuite

class Used1 extends BaseUsed1{
  test("deadCode1") {
    ???
  }
}
class Used2 extends BaseUsed2

class Unused extends BaseUnused{
}

class BaseUsed1 extends AnyFunSuite
class BaseUsed2 extends Suite
class BaseUnused

