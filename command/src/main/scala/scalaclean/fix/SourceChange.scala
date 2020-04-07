package scalaclean.fix

import scala.collection.immutable.TreeSet
import scala.meta.tokens.Token

object SourceChange {
  val none: SourceChange = SourceChanges(TreeSet.empty[Replace])
  def replaceTokens(tokens: Seq[Token],replacement: String, comment: String ): SourceChange = {
    assert (tokens.groupBy(_.input).size <= 1)
    val sorted = tokens.sortBy(_.start)
    sorted.grouped(2).foreach{
      case Seq(t1, t2) => assert(t2.start == t1.end)
    }
    Replace(sorted.head.start, sorted.last.end, replacement, comment)
  }
  def removeTokens(tokens: Seq[Token], comment: String ): SourceChange = {
    replaceTokens(tokens, "", comment)
  }
  def insertBefore(token: Token, text: String, comment: String ): SourceChange = {
    Replace(token.start,token.start, text, comment)
  }

  private final case class SourceChanges(changes: TreeSet[Replace]) extends SourceChange {
    override def +(change: SourceChange): SourceChange = change match {
      case SourceChanges(otherChanges) => SourceChanges(this.changes ++ otherChanges)
      case change: Replace =>SourceChanges(this.changes + change)
    }
  }
  private final case class Replace(start: Int, end:Int, replace: String, comment: String) extends SourceChange with Ordered[Replace] {
    override def +(change: SourceChange): SourceChange = change match {
      case SourceChanges(otherChanges) => SourceChanges(otherChanges + this)
      case change: Replace =>SourceChanges(TreeSet(this,change))
    }

    override def compare(that: Replace): Int = this.start - that.start
  }
}
sealed trait SourceChange {
  def + (change: SourceChange): SourceChange
}

