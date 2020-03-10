package pkg

object Chars {
  final val LF                        = '\u000A' // this is constant-folded but not dead code
  def isLF(c: Char)                   = c == LF
  def main(args: Array[String]): Unit = println(isLF(args(0).charAt(0)))
}
