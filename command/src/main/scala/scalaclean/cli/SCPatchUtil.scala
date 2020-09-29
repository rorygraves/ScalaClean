package scalaclean.cli

import scalaclean.model.SCPatch

object SCPatchUtil {

  def applyFixes(source: String, fixes: Seq[SCPatch]): String = {
    val sb         = new StringBuilder
    var currentPos = 0
    var remaining  = source

    val debug = false

    if (debug) println(s"-- ${remaining.length} remaining")

    fixes.foreach { case SCPatch(start, end, text, _) =>
      if (debug)
        println(
          s" start $start  end $end text '$text' curPos = $currentPos  remaining= ${remaining.length}  buffer = ${sb.length}"
        )
      if (start > currentPos) {
        val diff = start - currentPos
        if (debug) println(s"--  Taking $diff characters")
        sb.append(remaining.take(diff))
        remaining = remaining.drop(diff)
        currentPos = start
      }

      val toDrop = end - start
      //        sb.append(">>>>>")
      if (debug) println(s"--  dropping $toDrop chars - ${remaining.length} remaining '${remaining.take(toDrop)}''")
      //        sb.append(remaining.take(toDrop))
      remaining = remaining.drop(toDrop)
      //        sb.append("<<<<<")
      sb.append(text)
      currentPos = end
    }

    if (debug) println("adding remaining " + remaining)
    sb.append(remaining)

    val result = sb.toString
    if (debug) println("-------------------------")
    if (debug) println(result)
    if (debug) println("-------------------------")

    result
  }

}
