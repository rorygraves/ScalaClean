package junitentry

import org.junit._

class BaseUsed
class BaseUnused

class UsedChild extends Used1
class UnusedChild extends Unused

class Unused extends BaseUnused{
  def used = anotherUsed
  def anotherUsed = ???
  def unused = ???
}

class Used1 extends BaseUsed{
  @Test def used = anotherUsed
  def anotherUsed = ???
  def unused = ???
}
class Used2 extends BaseUsed{
  @Ignore def used = anotherUsed
  def anotherUsed = ???
  def unused = ???
}
class Used3 extends BaseUsed{
  @Before def used = anotherUsed
  def anotherUsed = ???
  def unused = ???
}
class Used4 extends BaseUsed{
  @After def used = anotherUsed
  def anotherUsed = ???
  def unused = ???
}
object Used5 {
  @BeforeClass def used = anotherUsed
  def anotherUsed = ???
  def unused = ???
}
object Used6 {
  @AfterClass def used = anotherUsed
  def anotherUsed = ???
  def unused = ???
}
