package scalaclean.util

import scala.meta.tokens.{Token, Tokens}

object TokenHelper {

  /** Walking back from a given Token, capture all whitespace tokens immediately before.
    *
    * @param targetToken The target token
    * @param tokens      The entire document of tokens
    * @return A sequence of tokens representing the whitespace immediately preceding the target token.
    */
  def whitespaceTokensBefore(targetToken: Token, tokens: Tokens): List[Token] = {
    whitespaceOrCommentsBefore(targetToken, tokens, false, false, false)
  }

  /** Walking back from a given Token, capture all whitespace tokens, or comments of the specified type immediately before.
    *
    * @param targetToken The target token
    * @param tokens      The entire document of tokens
    * @return A sequence of tokens representing the whitespace immediately preceding the target token.
    */
  def whitespaceOrCommentsBefore(targetToken: Token, tokens: Tokens,
                                 includeSingleLine: Boolean = true,
                                 includeMultiLine: Boolean = true,
                                 includeDocComment: Boolean = true): List[Token] = {
    def isWhiteSpaceOrComment(token: Token): Boolean = {
      token match {
        case _: Token.Space => true
        case _: Token.Tab => true
        case _: Token.CR => true
        case _: Token.LF => true
        case _: Token.FF => true
        case c: Token.Comment =>
          //cant use c.value as that is the body of the comment
          val real = c.toString()
          (includeSingleLine && real.startsWith("//")) ||
            (includeDocComment && real.startsWith("/**")) ||
            (includeMultiLine && real.startsWith("/*") && !real.startsWith("/**"))
        case _ => false
      }
    }

    // This is a bit ugly but pretty efficient as 'tokens' might be huge.
    val idx = tokens.indexOf(targetToken)
    if (idx == -1)
      List.empty
    else {
      var curIndex = idx - 1
      while (curIndex >= 0 && isWhiteSpaceOrComment(tokens(curIndex))) {
        curIndex -= 1
      }

      tokens.slice(curIndex + 1, idx).toList
    }
  }
}
