package encoding

// a test that we can navigate different encodings, and offset are managed appropriately
//all of the related projects should have the same code but with different encodings and some back ticks thrown in
object Encoding extends App {
  println(UsedObject.used)
}

// some text that has special characters * é ì
object UsedObject {
  val used = xxɐɎUsed + xxɐɎUsed2 + `also used` + `also used ɐɎ`

  /**
    * some text that has special characters
    * é ì
    */
  def xxɐɎUsed = 0
  def xxɐɎUnused = 0
  def xxɐɎUsed2 = 0

  def `also used` = 2
  def `foo bar Unused ɐɎ` = 2
  def `also used ɐɎ` = 2


}